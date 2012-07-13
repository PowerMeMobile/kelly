-module(k_statistic_msg_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	store_msg_stats/3
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("application.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(msg_stats_manifest, {
}).

-record(state, {
	tick_ref :: reference(),
	prefix_network_id_map :: [{string(), network_id()}],
	manifest :: #msg_stats_manifest{}
}).

-record(msg_stats, {
	input_id  :: msg_id(),
	msg_info  :: #msg_info{},
	req_time  :: integer()
}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_msg_stats(msg_id(), #msg_info{}, integer()) -> ok | {error, any()}.
store_msg_stats(InputId, MsgInfo, Time) ->
	gen_server:cast(?SERVER, {store_msg_stats, InputId, MsgInfo, Time}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	ok = k_mnesia_schema:ensure_table(
		msg_stats,
		record_info(fields, msg_stats)
	),

	TickRef = make_ref(),
	setup_alarm(TickRef),

	{ok, Manifest} = read_manifest(),

	?log_debug("started", []),
	{ok, #state{
		tick_ref = TickRef,
		prefix_network_id_map = [],
		manifest = Manifest
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({store_msg_stats, InputId, MsgInfo, Time}, State = #state{}) ->
	F = fun() ->
			Rec = #msg_stats{
				input_id = InputId,
				msg_info = MsgInfo,
				req_time = Time
			},
			mnesia:write(Rec)
		end,
	ok = try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State};

handle_cast({build_reports_and_delete_interval, Start, End, ReportPath1, ReportPath2}, State = #state{
	prefix_network_id_map = PrefixNetworkIdMap
}) ->
	F = fun() ->
			MatchHead = #msg_stats{req_time = '$1', _ = '_'},
	        GuardStart = {'>=', '$1', Start},
			GuardEnd = {'=<', '$1', End},
	        Result = '$_',
    	    MsgInfoRecs = mnesia:select(msg_stats, [{MatchHead, [GuardStart, GuardEnd], [Result]}]),

			{RawReport, NewPrefixNetworkIdMap} = lists:foldl(
				fun(#msg_stats{msg_info = MsgInfo}, {SoFar, Map}) ->
					MessageId = MsgInfo#msg_info.id,
					CustomerId = MsgInfo#msg_info.customer_id,
					Address = match_addr(MsgInfo#msg_info.dest_addr),

					case cache_lookup_network_id(Address, Map) of
						{value, NetworkId} ->
							{[{CustomerId, NetworkId, MessageId} | SoFar], Map};
						false ->
							case get_network_id(CustomerId, Address) of
								{ok, {Prefix, NetworkId}} ->
									{[{CustomerId, NetworkId, MessageId} | SoFar], [{Prefix, NetworkId} | Map]};
								{error, Reason} ->
									%% unusual case. log the error, and leave the state unchanged.
									?log_error("Impossible to determine NetworkId from CustomerId: ~p Address: ~p with reason: ~p",
										[CustomerId, Address, Reason]),
									{SoFar, Map}
							end
					end
				end, {[], PrefixNetworkIdMap}, MsgInfoRecs),
			%?log_debug("Raw msg stats report: ~p", [RawReport]),

			Report1 = k_statistic_reports:msg_stats_report1(RawReport),
			%?log_debug("Msg stats report1: ~p", [Report1]),

			Report2 = k_statistic_reports:msg_stats_report2(RawReport),
			%?log_debug("Msg stats report2: ~p", [Report2]),

			ok = k_statistic_util:write_term_to_file(Report1, ReportPath1),
			ok = k_statistic_util:write_term_to_file(Report2, ReportPath2),

			lists:foreach(fun(MsgInfoRec) ->
							mnesia:delete_object(MsgInfoRec)
						  end, MsgInfoRecs),
			{ok, NewPrefixNetworkIdMap}
		end,
	{ok, NewPrefixNetworkIdMap} =
		try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State#state{prefix_network_id_map = NewPrefixNetworkIdMap}};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({tick, TickRef}, State = #state{tick_ref = TickRef}) ->
	{ok, NewState} = on_tick(State),
	setup_alarm(TickRef),
	{noreply, NewState};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	mnesia:stop(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

setup_alarm(TickRef) ->
	TimerInterval = k_statistic_reports:stats_report_frequency() * 1000,
	timer:send_after(TimerInterval, self(), {tick, TickRef}).

on_tick(State = #state{}) ->
	Current = k_datetime:utc_unix_epoch(),
	Frequency = k_statistic_reports:stats_report_frequency(),
	%% Align time by frequency.
	To = Current - Current rem Frequency,
	From = To - Frequency,
	%?log_debug("~p-~p", [From, To]),
	Filename1 = io_lib:format("~p-1.dat", [From]),
	Filename2 = io_lib:format("~p-2.dat", [From]),
	Path1 = k_statistic_util:msg_stats_file_path(Filename1),
	Path2 = k_statistic_util:msg_stats_file_path(Filename2),
	build_reports_and_delete_interval(From, To, Path1, Path2),
	{ok, State}.

read_manifest() ->
	{ok, #msg_stats_manifest{}}.

build_reports_and_delete_interval(Start, End, ReportPath1, ReportPath2) ->
	gen_server:cast(?SERVER, {build_reports_and_delete_interval, Start, End, ReportPath1, ReportPath2}).

-spec match_addr(AddrRec::#'FullAddr'{} | #'FullAddrAndRefNum'{}) -> Addr::string().
match_addr(AddrRec) ->
	case AddrRec of
		#'FullAddr'{addr = Addr} ->
			Addr;
		#'FullAddrAndRefNum'{fullAddr = #'FullAddr'{addr = Addr}} ->
			Addr
	 end.

-spec cache_lookup_network_id(Address::string(), CacheMap::[{Prefix::string(), NetworkId::network_id()}]) ->
	{value, NetworkId::network_id()} | false.
cache_lookup_network_id(Address, CacheMap) ->
	case findwith(fun({Prefix, _}) -> lists:prefix(Prefix, Address) end, CacheMap) of
		{value, {_, NetworkId}} ->
			{value, NetworkId};
		false ->
			false
	end.

-spec findwith(fun((A::term()) -> boolean()), [A::term()]) -> {value, A::term()} | false.
findwith(_, []) ->
	false;
findwith(Pred, [H|T]) ->
	case Pred(H) of
		true ->
			{value, H};
		false ->
			findwith(Pred, T)
	end.

-spec get_network_id(CustomerId::customer_id(), Address::string()) -> {ok, {Prefix::string(), network_id()}} | {error, Reason::any()}.
get_network_id(CustomerId, Address) ->
	case k_aaa:get_customer_by_id(CustomerId) of
		{ok, #customer{networks = NetworkIds}} ->
			IdNetworkPairs = get_networks_by_ids(NetworkIds),
			case [{Prefix, NetworkId} || {true, {Prefix, NetworkId}} <-
						lists:map(fun({NetworkId, Network}) ->
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
	lists:foldl(fun(NetworkId, SoFar) ->
				   case k_config:get_network(NetworkId) of
						{ok, Network = #network{}} ->
							[{NetworkId, Network} | SoFar];
						_ ->
							SoFar
					end
				end, [], NetworkIds).

-spec does_address_match_network(Address::string(), Network::#network{}) -> {true, CodeAndPrefix::string()} | false.
does_address_match_network(Address, #network{
	countryCode = CountryCode,
	prefixes = Prefixes
}) ->
	CodeAndPrefixes = lists:map(fun(Prefix) ->
									CountryCode ++ Prefix
								end, Prefixes),
	case findwith(fun(CodeAndPrefix) -> lists:prefix(CodeAndPrefix, Address) end, CodeAndPrefixes) of
		{value, Prefix} ->
			{true, Prefix};
		false ->
			false
	end.
