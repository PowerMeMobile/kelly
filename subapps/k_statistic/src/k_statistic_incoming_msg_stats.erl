-module(k_statistic_incoming_msg_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	store_incoming_msg_stats/3,
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
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(msg_stats_manifest, {
}).

-record(state, {
	tick_ref :: reference(),
	manifest :: #msg_stats_manifest{}
}).

-record(incoming_msg_stats, {
	output_id  :: msg_id(),
	msg_info  :: #msg_info{},
	time  :: integer()
}).

-define(SERVER, ?MODULE).
-define(TABLE, incoming_msg_stats).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_incoming_msg_stats(msg_id(), #msg_info{}, integer()) -> ok | {error, any()}.
store_incoming_msg_stats(OutputId, MsgInfo, Time) ->
	gen_server:cast(?SERVER, {store_incoming_msg_stats, OutputId, MsgInfo, Time}).

-spec delete_all() -> ok.
delete_all() ->
	gen_server:cast(?SERVER, {delete_all}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	ok = k_mnesia_schema:ensure_table(
		incoming_msg_stats,
		record_info(fields, incoming_msg_stats)
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

handle_cast({store_incoming_msg_stats, OutputId, MsgInfo, Time}, State = #state{}) ->
	F = fun() ->
			Rec = #incoming_msg_stats{
				output_id = OutputId,
				msg_info = MsgInfo,
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

handle_cast({build_reports_and_delete_interval, Start, End}, State = #state{}) ->
	Filename = io_lib:format("~p.dat", [Start]),
	ReportPath = k_statistic_util:incoming_msg_stats_file_path(Filename),

	F = fun() ->
			Records = mnesia:select(?TABLE, ets:fun2ms(
				fun(Record = #incoming_msg_stats{time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
		    )),

			%% store raw generic records.
			ok = k_statistic_util:write_term_to_file(Records, ReportPath),

			%% delete stored records.
			lists:foreach(fun mnesia:delete_object/1, Records)
		end,
	ok =
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
	build_reports_and_delete_interval(From, To),
	{ok, State}.

read_manifest() ->
	{ok, #msg_stats_manifest{}}.

build_reports_and_delete_interval(Start, End) ->
	gen_server:cast(?SERVER, {build_reports_and_delete_interval, Start, End}).
