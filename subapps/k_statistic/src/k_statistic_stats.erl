-module(k_statistic_stats).

-behaviour(gen_server).

%% API
-export([
	start_link/0,

	store_outgoing_msg_stats/3,
	delete_outgoing_msg_stats/0,

	store_incoming_msg_stats/3,
	delete_incoming_msg_stats/0,

	store_status_stats/5,
	delete_status_stats/0
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
-include("msg_stats.hrl").
-include("status_stats.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(stats_manifest, {
}).

-record(state, {
	tick_ref :: reference(),
	manifest :: #stats_manifest{}
}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec store_outgoing_msg_stats(msg_id(), #msg_info{}, integer()) -> ok | {error, any()}.
store_outgoing_msg_stats(InputId, MsgInfo, Time) ->
	gen_server:cast(?SERVER, {store_outgoing_msg_stats, InputId, MsgInfo, Time}).

-spec delete_outgoing_msg_stats() -> ok.
delete_outgoing_msg_stats() ->
	gen_server:cast(?SERVER, {delete_outgoing_msg_stats}).

-spec store_incoming_msg_stats(msg_id(), #msg_info{}, integer()) -> ok | {error, any()}.
store_incoming_msg_stats(OutputId, MsgInfo, Time) ->
	gen_server:cast(?SERVER, {store_incoming_msg_stats, OutputId, MsgInfo, Time}).

-spec delete_incoming_msg_stats() -> ok.
delete_incoming_msg_stats() ->
	gen_server:cast(?SERVER, {delete_incoming_msg_stats}).

-spec store_status_stats(msg_id(), msg_id(), #msg_info{}, status(), unix_epoch()) -> ok | {error, any()}.
store_status_stats(InputId, OutputId, MsgInfo, Status, Time) ->
	gen_server:cast(?SERVER, {store_status_stats, InputId, OutputId, MsgInfo, Status, Time}).

-spec delete_status_stats() -> ok.
delete_status_stats() ->
	gen_server:cast(?SERVER, {delete_status_stats}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	ok = k_mnesia_schema:ensure_table(
		outgoing_msg_stats,
		msg_stats,
		record_info(fields, msg_stats)
	),

	ok = k_mnesia_schema:ensure_table(
		incoming_msg_stats,
		msg_stats,
		record_info(fields, msg_stats)
	),

	ok = k_mnesia_schema:ensure_table(
		status_stats,
		record_info(fields, status_stats)
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

handle_cast({store_outgoing_msg_stats, InputId, MsgInfo, Time}, State = #state{}) ->
	F = fun() ->
			Rec = #msg_stats{
				id = InputId,
				msg_info = MsgInfo,
				time = Time
			},
			mnesia:write(outgoing_msg_stats, Rec, write)
		end,
	ok = try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State};

handle_cast({delete_outgoing_msg_stats}, State = #state{}) ->
	F = fun() ->
			Tab = outgoing_msg_stats,
			[mnesia:delete(Tab, Key, sticky_write) || Key <- mnesia:all_keys(Tab)]
		end,
	mnesia:transaction(F),
	{noreply, State};

handle_cast({store_incoming_msg_stats, OutputId, MsgInfo, Time}, State = #state{}) ->
	F = fun() ->
			Rec = #msg_stats{
				id = OutputId,
				msg_info = MsgInfo,
				time = Time
			},
			mnesia:write(incoming_msg_stats, Rec, write)
		end,
	ok = try
			mnesia:activity(sync_dirty, F)
		catch
			exit:Reason ->
				{error, Reason}
		end,
	{noreply, State};

handle_cast({delete_incoming_msg_stats}, State = #state{}) ->
	F = fun() ->
			Tab = incoming_msg_stats,
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

handle_cast({delete_status_stats}, State = #state{}) ->
	F = fun() ->
			Tab = status_stats,
			[mnesia:delete(Tab, Key, sticky_write) || Key <- mnesia:all_keys(Tab)]
		end,
	mnesia:transaction(F),
	{noreply, State};

handle_cast({store_outgoing_msg_stats_slice_and_delete_interval, Start, End}, State = #state{}) ->
	SlicePath = k_statistic_utils:msg_stats_slice_path(Start),

	F = fun() ->
			Records = mnesia:select(outgoing_msg_stats, ets:fun2ms(
				fun(Record = #msg_stats{time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
		    )),

			%% store raw generic records.
			ok = k_statistic_utils:write_term_to_file(Records, SlicePath),

			%% delete stored records.
			lists:foreach(
				fun(Record) ->
					mnesia:delete_object(outgoing_msg_stats, Record, write)
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

handle_cast({store_incoming_msg_stats_slice_and_delete_interval, Start, End}, State = #state{}) ->
	SlicePath = k_statistic_utils:incoming_msg_stats_slice_path(Start),

	F = fun() ->
			Records = mnesia:select(incoming_msg_stats, ets:fun2ms(
				fun(Record = #msg_stats{time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
		    )),

			%% store raw generic records.
			ok = k_statistic_utils:write_term_to_file(Records, SlicePath),

			%% delete stored records.
			lists:foreach(
				fun(Record) ->
					mnesia:delete_object(incoming_msg_stats, Record, write)
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
	{noreply, State#state{}};

handle_cast({store_status_stats_slice_and_delete_interval, Start, End}, State) ->
	SlicePath = k_statistic_utils:status_stats_slice_path(Start),

	F = fun() ->
    	    Records = mnesia:select(status_stats, ets:fun2ms(
				fun(Record = #status_stats{time = Time})
					when Time >= Start andalso Time =< End ->
						Record
				end
			)),

			%% store raw generic records.
			ok = k_statistic_utils:write_term_to_file(Records, SlicePath),

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
	TimerInterval = k_statistic_utils:stats_report_frequency() * 1000,
	timer:send_after(TimerInterval, self(), {tick, TickRef}).

on_tick(State = #state{}) ->
	Current = k_datetime:utc_unix_epoch(),
	Frequency = k_statistic_utils:stats_report_frequency(),
	%% Align time by frequency.
	To = Current - Current rem Frequency,
	From = To - Frequency,
	%?log_debug("~p-~p", [From, To]),
	store_slices_and_delete_intervals(From, To),
	{ok, State}.

read_manifest() ->
	{ok, #stats_manifest{}}.

store_slices_and_delete_intervals(Start, End) ->
	gen_server:cast(?SERVER, {store_outgoing_msg_stats_slice_and_delete_interval, Start, End}),
	gen_server:cast(?SERVER, {store_incoming_msg_stats_slice_and_delete_interval, Start, End}),
	gen_server:cast(?SERVER, {store_status_stats_slice_and_delete_interval, Start, End}).
