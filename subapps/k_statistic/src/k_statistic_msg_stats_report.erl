-module(k_statistic_msg_stats_report).

-export([
	get_report/3,
	build_msg_stats_report/2
]).

-include("msg_stats.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(
	ReportType::atom(),
	From::os:timestamp(),
	To::os:timestamp()
) -> {ok, term()} | {error, Reason::any()}.
get_report(ReportType, From, To) when From < To ->
	Filenames = k_statistic_utils:get_file_list_with(
		From, To, fun k_statistic_utils:msg_stats_slice_path/1),
	Records = k_statistic_utils:read_terms_from_files(Filenames),
	RawReport = build_raw_report(Records),
	Report = build_msg_stats_report(ReportType, RawReport),
	Annotate = annotate_msg_stats_report(ReportType, Report),

	AnnotatedReport = {ReportType, Annotate},
	{ok, AnnotatedReport}.

build_raw_report(Records) ->
	{RawReport, _NewPrefixNetworkIdMap} = lists:foldl(
		fun(#msg_stats{msg_info = MsgInfo}, {SoFar, PrefixNetworkIdMap}) ->
			MessageId = MsgInfo#msg_info.id,
			CustomerId = MsgInfo#msg_info.customer_id,
			DestAddr = match_addr(MsgInfo#msg_info.dst_addr),

			case cache_lookup_network_id(DestAddr, PrefixNetworkIdMap) of
				{value, NetworkId} ->
					{[{CustomerId, NetworkId, MessageId} | SoFar], PrefixNetworkIdMap};
				false ->
					case get_network_id(CustomerId, DestAddr) of
						{ok, {Prefix, NetworkId}} ->
							{
								[{CustomerId, NetworkId, MessageId} | SoFar],
								[{Prefix, NetworkId} | PrefixNetworkIdMap]
							};
						{error, Reason} ->
							%% unusual case. log the error, and leave the state unchanged.
							?log_error("Impossible to determine NetworkId from CustomerId: ~p Address: ~p with reason: ~p",								[CustomerId, DestAddr, Reason]),
							{SoFar, PrefixNetworkIdMap}
					end
			end
		end,
		{[], []},
		Records),
	RawReport.

-spec build_msg_stats_report(ReportType::atom(), Records::[tuple()]) -> [tuple()].
build_msg_stats_report(customers, Records) ->
	GroupByCustomer = msg_stats_report(1, Records),
	GroupByCustomerAndNetwork = lists:map(
		fun({K, List}) ->
			{K, msg_stats_report(1, List)}
		end,
		GroupByCustomer),
	GroupByCustomerAndNetwork;

build_msg_stats_report(networks, Records) ->
	GroupByNetwork = msg_stats_report(2, Records),
	GroupByNetworkAndCustomer = lists:map(
		fun({K, List}) ->
			{K, msg_stats_report(1, List)}
		end,
		GroupByNetwork),
	GroupByNetworkAndCustomer.

%% ===================================================================
%% Internal
%% ===================================================================

-spec annotate_msg_stats_report(ReportType::atom(), Records::[tuple()]) -> [tuple()].
annotate_msg_stats_report(customers, Customers) ->
	lists:map(
		fun(Customer) ->
			{CustomerId, Networks} = Customer,
			[
				{id, list_to_binary(uuid:to_string(CustomerId))},
				{networks,
					lists:map(
						fun({NetworkId, MsgIds}) ->
							[
								{id, list_to_binary(uuid:to_string(NetworkId))},
								{mids, MsgIds}
							]
						end,
						Networks)}
			]
		end,
		Customers);

annotate_msg_stats_report(networks, Networks) ->
	lists:map(
		fun({NetworkId, Customers}) ->
			[
				{id, list_to_binary(uuid:to_string(NetworkId))},
				{customers,
					lists:map(
						fun({CustomerId, MsgIds}) ->
							[
								{id, list_to_binary(uuid:to_string(CustomerId))},
								{mids, MsgIds}
							]
						 end,
						Customers)}
			]
		end,
		Networks).


-spec msg_stats_report(KeyN::integer(), [tuple()]) -> [tuple()].
msg_stats_report(KeyN, Records) ->
	Pairs = lists:map(
		fun(R) ->
			k_statistic_utils:make_pair(KeyN, R)
		end,
		Records),
	Dict = lists:foldl(
		fun({K, V}, Dict) ->
			orddict:append_list(K, [V], Dict)
		end,
		orddict:new(),
		Pairs),
	List = orddict:to_list(Dict),
	List.

-spec match_addr(AddrRec::#full_addr{} | #full_addr_ref_num{}) -> Addr::string().
match_addr(AddrRec) ->
	case AddrRec of
		#full_addr{addr = Addr} ->
			Addr;
		#full_addr_ref_num{full_addr = #full_addr{addr = Addr}} ->
			Addr
	 end.

-spec cache_lookup_network_id(Address::string(), CacheMap::[{Prefix::string(), NetworkId::network_id()}]) ->
	{value, NetworkId::network_id()} | false.
cache_lookup_network_id(Address, CacheMap) ->
	Result = k_statistic_utils:findwith(
		fun({Prefix, _}) ->
			lists:prefix(Prefix, binary_to_list(Address))
		end,
		CacheMap),
	case  Result of
		{value, {_, NetworkId}} ->
			{value, NetworkId};
		false ->
			false
	end.

-spec get_network_id(CustomerId::customer_id(), Address::string()) -> {ok, {Prefix::string(), network_id()}} | {error, Reason::any()}.
get_network_id(CustomerId, Address) ->
	case k_aaa:get_customer_by_id(CustomerId) of
		{ok, #customer{networks = NetworkIds}} ->
			IdNetworkPairs = get_networks_by_ids(NetworkIds),
			case [{Prefix, NetworkId} || {true, {Prefix, NetworkId}} <-
						lists:map(
							fun({NetworkId, Network}) ->
								case does_address_match_network(Address, Network) of
									{true, Prefix} ->
										{true, {Prefix, NetworkId}};
									false ->
										false
								end
							end,
						IdNetworkPairs)]
			of
				[{Prefix, NetworkId} | _] ->
					{ok, {Prefix, NetworkId}};
				[] ->
					{error, no_entry}
			end;
		Error ->
			Error
	end.

-spec get_networks_by_ids(NetworkIds::[network_id()]) -> [{network_id(), #network{}}].
get_networks_by_ids(NetworkIds) ->
	lists:foldl(
		fun(NetworkId, SoFar) ->
			case k_config:get_network(NetworkId) of
				{ok, Network = #network{}} ->
					[{NetworkId, Network} | SoFar];
				_ ->
					SoFar
				end
		end,
		[],
		NetworkIds).

-spec does_address_match_network(Address::string(), Network::#network{}) -> {true, CodeAndPrefix::string()} | false.
does_address_match_network(Address, #network{
	countryCode = CountryCode,
	prefixes = Prefixes
}) ->
	CodeAndPrefixes = lists:map(
		fun(Prefix) ->
			binary_to_list(CountryCode) ++ binary_to_list(Prefix)
		end,
		Prefixes),
	Result = k_statistic_utils:findwith(
		fun(CodeAndPrefix) ->
			lists:prefix(CodeAndPrefix, binary_to_list(Address))
		end,
		CodeAndPrefixes),
	case Result of
		{value, Prefix} ->
			{true, Prefix};
		false ->
			false
	end.
