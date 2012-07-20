-module(k_statistic_msg_stats_report).

-export([
	msg_stats_report/3,
	build_msg_stats_report/2
]).

%% ===================================================================
%% API
%% ===================================================================

-spec msg_stats_report(
	ReportType::atom(),
	From::os:timestamp(),
	To::os:timestamp()
) -> {ok, term()} | {error, Reason::any()}.
msg_stats_report(ReportType, From, To) when From < To ->
	Filenames = k_statistic_utils:get_file_list(From, To,
		fun(Timestamp) ->
			k_statistic_utils:msg_stats_slice_path(Timestamp, ReportType)
		end),
	Reports =
		lists:foldr(
		  fun(Filename, SoFar) ->
				  case k_statistic_utils:read_term_from_file(Filename) of
					  {ok, []} ->
						  SoFar;
					  {ok, Records} ->
						  AnnotatedRecords = annotate_msg_stats_report(ReportType, Records),
						  AnnotatedRecords ++ SoFar;
					  {error, _Reason} ->
						  %?log_debug("Missing msg stats report: ~p", [Filename])
						  SoFar
				  end
		  end, [], Filenames),
	AnnotatedReport = {ReportType, Reports},
	{ok, AnnotatedReport}.

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
				{id, CustomerId},
				{networks,
					lists:map(
						fun({NetworkId, MsgIds}) ->
							[
								{id, NetworkId},
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
				{id, NetworkId},
				{customers,
					lists:map(
						fun({CustomerId, MsgIds}) ->
							[
								{id, CustomerId},
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
