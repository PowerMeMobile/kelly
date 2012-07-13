-module(k_statistic_status_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	store_status_stats/5,

	delete_all/0,
	build_reports_and_delete_interval/2
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
-include("status_stats.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(status_stats_manifest, {
}).

-record(state, {
	tick_ref :: reference(),
	manifest :: #status_stats_manifest{}
}).

-define(SERVER, ?MODULE).
-define(TABLE, status_stats).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_status_stats(msg_id(), msg_id(), #msg_info{}, status(), unix_epoch()) -> ok | {error, any()}.
store_status_stats(InputId, OutputId, MsgInfo, Status, Time) ->
	gen_server:cast(?SERVER, {store_status_stats, InputId, OutputId, MsgInfo, Status, Time}).

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

	{ok, Manifest} = read_manifest(),

	?log_debug("started", []),
	{ok, #state{
		tick_ref = TickRef,
		manifest = Manifest
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

handle_cast({store_status_stats, InputId, OutputId, MsgInfo, MsgStatus, Time}, State = #state{}) ->
	F = fun() ->
			Rec = #status_stats{
				input_id = InputId,
				output_id = OutputId,
				msg_info = MsgInfo,
				msg_status = MsgStatus,
				time = Time
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

% k_statistic_status_stats:build_reports_and_delete_interval(1341842975, 1341842984).

handle_cast({build_reports_and_delete_interval, Start, End}, State) ->
	F = fun() ->
    	    Records = mnesia:select(?TABLE, ets:fun2ms(
				fun(Record = #status_stats{time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
			)),

			Filename = io_lib:format("~p.dat", [Start]),
			ReportPath = k_statistic_util:status_stats_file_path(Filename),
			ok = k_statistic_util:write_term_to_file(Records, ReportPath),

			lists:foreach(
				fun(Record) ->
					mnesia:delete_object(Record)
				end,
				Records)
		end,
	ok =
		try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State};

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
	build_reports_and_delete_interval(From, To),
	{ok, State}.

read_manifest() ->
	{ok, #status_stats_manifest{}}.

build_reports_and_delete_interval(Start, End) ->
	gen_server:cast(?SERVER, {build_reports_and_delete_interval, Start, End}).

