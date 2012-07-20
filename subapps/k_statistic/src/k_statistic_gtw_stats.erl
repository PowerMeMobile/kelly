-module(k_statistic_gtw_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	store_gtw_stats/3,
	delete_all/0
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
-include_lib("stdlib/include/ms_transform.hrl").

-record(gtw_stats, {
	gateway_id :: gateway_id(),
	number :: integer(),
 	init_time  :: integer()
}).

-record(state, {
	tick_ref :: reference()
}).

-define(SERVER, ?MODULE).
-define(TABLE, gtw_stats).


%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_gtw_stats(gateway_id(), integer(), integer()) -> ok | {error, any()}.
store_gtw_stats(GatewayId, Number, Time) ->
	gen_server:cast(?SERVER, {store_gtw_stats, GatewayId, Number, Time}).

-spec delete_all() -> ok.
delete_all() ->
	gen_server:cast(?SERVER, {delete_all}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	ok = k_mnesia_schema:ensure_table(
		?TABLE,
		record_info(fields, ?TABLE)
	),

	TickRef = make_ref(),
	setup_alarm(TickRef),

	?log_debug("started", []),
	{ok, #state{
		tick_ref = TickRef
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({delete_all}, State = #state{}) ->
	F = fun() ->
			Tab = ?TABLE,
			[mnesia:delete(Tab, Key, sticky_write) || Key <- mnesia:all_keys(Tab)]
		end,
	mnesia:transaction(F),
	{noreply, State};

handle_cast({store_gtw_stats, GatewayId, Number, Time}, State = #state{}) ->
	F = fun() ->
			NewGtwStats = case mnesia:read(?TABLE, GatewayId, write) of
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

handle_cast({build_report_and_delete_interval, Start, End}, State = #state{}) ->
	ReportPath = k_statistic_util:gtw_stats_slice_path(Start),

	F = fun() ->
			Records = mnesia:select(?TABLE, ets:fun2ms(
				fun(Record = #gtw_stats{init_time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
			)),

			%?log_debug("Raw gtw stats report: ~p", [Records]),

			%% build & store the report.
			Report = k_statistic_reports:gtw_stats_report(Records),
			ok = k_statistic_util:write_term_to_file(Report, ReportPath),
			%?log_debug("Gtw report: ~p", [Report]),

			%% delete stored records.
			lists:foreach(fun mnesia:delete_object/1, Records)
		end,
	ok = try
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
	build_report_and_delete_interval(From, To),
	{ok, State}.

build_report_and_delete_interval(Start, End) ->
	gen_server:cast(?SERVER, {build_report_and_delete_interval, Start, End}).
