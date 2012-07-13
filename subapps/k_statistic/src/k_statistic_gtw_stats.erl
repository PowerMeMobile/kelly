-module(k_statistic_gtw_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	store_gtw_stats/3
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
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(gtw_stats, {
	gateway_id :: gateway_id(),
	number :: integer(),
 	init_time  :: integer()
}).

-record(state, {
	tick_ref :: reference()
}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_gtw_stats(gateway_id(), integer(), integer()) -> ok | {error, any()}.
store_gtw_stats(GatewayId, Number, Time) ->
	gen_server:cast(?SERVER, {store_gtw_stats, GatewayId, Number, Time}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	ok = k_mnesia_schema:ensure_table(
		gtw_stats,
		record_info(fields, gtw_stats)
	),

	TickRef = make_ref(),
	setup_alarm(TickRef),

	?log_debug("started", []),
	{ok, #state{
		tick_ref = TickRef
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({store_gtw_stats, GatewayId, Number, Time}, State = #state{}) ->
	F = fun() ->
			NewGtwStats = case mnesia:read(gtw_stats, GatewayId, write) of
							[] ->
								#gtw_stats{
									gateway_id = GatewayId,
									number = Number,
									init_time = Time
								};
							[GtwStats] ->
								NewNumber = GtwStats#gtw_stats.number + Number,
								GtwStats#gtw_stats{number = NewNumber}
    					 end,
			mnesia:write(NewGtwStats)
		end,
	{atomic, ok} = mnesia:transaction(F),
	{noreply, State};

handle_cast({build_report_and_delete_interval, _Start, _End, ReportPath}, State = #state{}) ->
	F = fun() ->
			Query = qlc:q([GtwStats || GtwStats <- mnesia:table(gtw_stats)]),
			GtwStatsRecs = qlc:e(Query),

			%?log_debug("Raw gtw stats report: ~p", [GtwStatsRecs]),

			Report = k_statistic_reports:gtw_stats_report(GtwStatsRecs),
			%?log_debug("Gtw report: ~p", [Report]),

			ok = k_statistic_util:write_term_to_file(Report, ReportPath),

			lists:foreach(fun(GtwStatsRec) ->
							mnesia:delete_object(GtwStatsRec)
						  end, GtwStatsRecs)
		end,

		try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State#state{}};

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
	Filename = io_lib:format("~p.dat", [From]),
	Path = k_statistic_util:gtw_stats_file_path(Filename),
	build_report_and_delete_interval(From, To, Path),
	{ok, State}.

build_report_and_delete_interval(Start, End, ReportPath) ->
	gen_server:cast(?SERVER, {build_report_and_delete_interval, Start, End, ReportPath}).
