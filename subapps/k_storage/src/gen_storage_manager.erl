-module(gen_storage_manager).

-behaviour(gen_server).

%% API
-export([
	start_link/0,

	get_storage_mode/0,
	get_shifts/0
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
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-type event_name() :: 'ShiftEvent' | atom().
-type storage_mode() :: 'RegularMode' | atom().
-type server_name() :: mondodb_storage:server_name().
-type plist() :: [{atom(), term()}].
-type spec_state() :: term().
-type reason() :: term().

%% ===================================================================
%% gen_storage_manager callbacks
%% ===================================================================

-callback ensure_static_storage_indexes(server_name()) -> ok.
-callback ensure_dynamic_storage_indexes(server_name()) -> ok.
-callback new_spec_state(plist()) -> spec_state().
-callback decode_spec_state(bson:document()) -> spec_state().
-callback encode_spec_state(spec_state()) -> bson:document().
-callback next_mode_event(
	storage_mode(), event_name(), calendar:datetime(), calendar:datetime(), spec_state()
) ->
	{storage_mode(), event_name(), calendar:datetime(), spec_state()}.

-record(state, {
	timer_ref :: reference(),
	shift_frame,

	curr_shift_time :: calendar:datetime(),
	next_shift_time :: calendar:datetime(),

	shifts :: [binary()],

	curr_mode :: storage_mode(),
	next_event :: event_name(),
	next_event_time :: calendar:datetime(),

	spec_state :: spec_state()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_storage_mode() -> {ok, storage_mode()} | {error, reason()}.
get_storage_mode() ->
	{Pid, _} = gproc:await({n, l, ?MODULE}),
	gen_server:call(Pid, get_curr_mode).

-spec get_shifts() -> {ok, [binary()]}.
get_shifts() ->
	{Pid, _} = gproc:await({n, l, ?MODULE}),
	gen_server:call(Pid, get_shifts).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	%% initialize static storage.
	ok = start_static_storage(),

	CurrTime = k_datetime:utc_datetime(),

	{ok, NewState} =
		 case read_storage_state() of
			{ok, State} ->
				%% a good place to correct the state.
				{ok, State};
			{error, no_entry} ->
				make_storage_state(CurrTime)
		end,
	ok = write_storage_state(NewState),

	CurrMode = NewState#state.curr_mode,
	ok = start_dynamic_storage(CurrMode),

	{ok, TimerRef} = start_heartbeat_timer(),

	%% signal events manager is ready.
	gproc:reg({n, l, ?MODULE}),

	{ok, NewState#state{timer_ref = TimerRef}}.

handle_call(get_curr_mode, _From, State = #state{
	curr_mode = CurrMode
}) ->
	{reply, {ok, CurrMode}, State};

handle_call(get_shifts, _From, State = #state{
	shifts = Shifts
}) ->
	{reply, {ok, Shifts}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({timeout, TimerRef, heartbeat}, State = #state{
	timer_ref = TimerRef,
	next_event = CurrEvent,
	next_event_time = CurrEventTime
}) ->
   	%?log_debug("~p", [State]),

	CurrTime = k_datetime:utc_datetime(),

	{ok, NewState} =
		case CurrTime >= CurrEventTime of
			true ->
				handle_event(CurrEvent, CurrTime, State);
			false ->
				{ok, State}
		end,

	{ok, NewTimerRef} = start_heartbeat_timer(),

	{noreply, NewState#state{
		timer_ref = NewTimerRef
	}};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

start_static_storage() ->
	{ok, StaticProps} = application:get_env(?APP, static_storage),

	{ok, Pid} = gen_storage_manager_sup:start_child([{server_name, static_storage} | StaticProps]),
	true = register(static_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, static_storage]),

	ok = k_storage_manager:ensure_static_storage_indexes(static_storage).

start_dynamic_storage('RegularMode') ->
	ok = start_curr_dynamic_storage();
start_dynamic_storage(_Other) ->
	ok = start_curr_dynamic_storage(),
	ok = start_prev_dynamic_storage().

start_curr_dynamic_storage() ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),

	ShiftDbName = curr_shift_db_name(),

	%% start current dynamic storage.
	CurrProps = [{server_name, curr_dynamic_storage}, {mongodb_dbname, ShiftDbName} | DynamicProps],

	{ok, Pid} = gen_storage_manager_sup:start_child(CurrProps),
	true = register(curr_dynamic_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, curr_dynamic_storage]),

	ok = k_storage_manager:ensure_dynamic_storage_indexes(curr_dynamic_storage).

start_prev_dynamic_storage() ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),

	ShiftDbName = prev_shift_db_name(),

	%% start previous dynamic storage.
	PrevProps = [{server_name, prev_dynamic_storage}, {mongodb_dbname, ShiftDbName} | DynamicProps],

	{ok, Pid} = gen_storage_manager_sup:start_child(PrevProps),
	true = register(prev_dynamic_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, prev_dynamic_storage]),

	ok = k_storage_manager:ensure_dynamic_storage_indexes(prev_dynamic_storage).

start_heartbeat_timer() ->
	TimerRef = erlang:start_timer(1000, self(), heartbeat),
	{ok, TimerRef}.

read_storage_state() ->
	case mongodb_storage:find_one(static_storage, 'storage.state', {}) of
		{ok, Doc} ->
			ShiftFrame = bsondoc:at(shift_frame, Doc),
			CurrShiftTime = bsondoc:at(curr_shift_time, Doc),
			NextShiftTime = bsondoc:at(next_shift_time, Doc),
			Shifts = bsondoc:at(shifts, Doc),
			CurrMode = bsondoc:binary_to_atom(bsondoc:at(curr_mode, Doc)),
			NextEvent = bsondoc:binary_to_atom(bsondoc:at(next_event, Doc)),
			NextEventTime = bsondoc:at(next_event_time, Doc),
			SpecState = k_storage_manager:decode_spec_state(bsondoc:at(spec_state, Doc)),

			State = #state{
				shift_frame = ShiftFrame,
				curr_shift_time = k_datetime:timestamp_to_datetime(CurrShiftTime),
				next_shift_time = k_datetime:timestamp_to_datetime(NextShiftTime),
				shifts = Shifts,
				curr_mode = CurrMode,
				next_event = NextEvent,
				next_event_time = k_datetime:timestamp_to_datetime(NextEventTime),
				spec_state = SpecState
			},
			{ok, State};
		Error ->
			Error
	end.

write_storage_state(#state{
	shift_frame = ShiftFrame,
	curr_shift_time = CurrShiftTime,
	next_shift_time = NextShiftTime,
	shifts = Shifts,
	curr_mode = CurrMode,
	next_event = NextEvent,
	next_event_time = NextEventTime,
	spec_state = SpecState
}) ->
	Modifier = {
		shift_frame     , ShiftFrame,
		curr_shift_time , k_datetime:datetime_to_timestamp(CurrShiftTime),
		next_shift_time , k_datetime:datetime_to_timestamp(NextShiftTime),
		shifts          , Shifts,
		curr_mode       , bsondoc:atom_to_binary(CurrMode),
		next_event      , bsondoc:atom_to_binary(NextEvent),
		next_event_time , k_datetime:datetime_to_timestamp(NextEventTime),
		spec_state      , k_storage_manager:encode_spec_state(SpecState)
	},
	ok = mongodb_storage:upsert(static_storage, 'storage.state', {}, Modifier).

make_storage_state(CurrTime) ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),

	ShiftFrame = proplists:get_value(shift_frame, DynamicProps),

	CurrShiftTime = gen_storage_manager_utils:get_curr_shift_time(CurrTime, ShiftFrame),
	NextShiftTime = gen_storage_manager_utils:get_next_shift_time(CurrTime, ShiftFrame),

	SpecState = k_storage_manager:new_spec_state(DynamicProps),

	NewShiftDbName = curr_shift_db_name(),

	%% it's a clear start, the mode and event are undefined.
	{NextMode, NextEvent, NextEventTime, NewSpecState} = k_storage_manager:next_mode_event(
		undefined, undefined, CurrShiftTime, NextShiftTime, SpecState
	),

	{ok, #state{
		shift_frame = ShiftFrame,
		curr_shift_time = CurrShiftTime,
		next_shift_time = NextShiftTime,
		shifts = [NewShiftDbName],
		curr_mode = NextMode,
		next_event = NextEvent,
		next_event_time = NextEventTime,
		spec_state = NewSpecState
	}}.

handle_event('ShiftEvent', CurrTime, State = #state{
	shift_frame = ShiftFrame,
	next_shift_time = CurrShiftTime,
	shifts = Shifts,
	curr_mode = 'RegularMode',
	spec_state = SpecState
}) ->
	NextShiftTime = gen_storage_manager_utils:get_next_shift_time(CurrTime, ShiftFrame),

	{NextMode, NextEvent, NextEventTime, NextSpecState} = k_storage_manager:next_mode_event(
		'RegularMode', 'ShiftEvent', CurrShiftTime, NextShiftTime, SpecState
	),
	?log_info("~p <- ~p => ~p ~p", ['RegularMode', 'ShiftEvent', NextMode, NextEvent]),

	ok = shift_storages(),
	NextShiftDbName = curr_shift_db_name(),

	NewState = State#state{
		curr_shift_time = CurrShiftTime,
		next_shift_time = NextShiftTime,
		shifts = [NextShiftDbName | Shifts],
		curr_mode = NextMode,
		next_event = NextEvent,
		next_event_time = NextEventTime,
		spec_state = NextSpecState
	},
	ok = write_storage_state(NewState),
	{ok, NewState};
handle_event(CurrEvent, _CurrTime, State = #state{
	next_shift_time = NextShiftTime,
	curr_shift_time = CurrShiftTime,
	curr_mode = CurrMode,
	spec_state = SpecState
}) ->
	{NextMode, NextEvent, NextEventTime, NextSpecState} = k_storage_manager:next_mode_event(
		CurrMode, CurrEvent, CurrShiftTime, NextShiftTime, SpecState
	),
	?log_info("~p <- ~p => ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),

	NewState = State#state{
		curr_mode = NextMode,
		next_event = NextEvent,
		next_event_time = NextEventTime,
		spec_state = NextSpecState
	},
	ok = write_storage_state(NewState),
	{ok, NewState}.

shift_storages() ->
	%% unregister and stop previous storage, if one.
	case whereis(prev_dynamic_storage) of
		undefined -> 'NOP';
		PrevPid ->
			true = unregister(prev_dynamic_storage),
			ok = mongodb_storage:stop(PrevPid)
	end,
	%% turn the current storage into previous.
	CurrPid = whereis(curr_dynamic_storage),
	true = unregister(curr_dynamic_storage),
	true = register(prev_dynamic_storage, CurrPid),
	%% start new current storage.
	ok = start_curr_dynamic_storage().

prev_shift_db_name() ->
	make_shift_db_name(fun gen_storage_manager_utils:get_prev_shift_time/2).

curr_shift_db_name() ->
	make_shift_db_name(fun gen_storage_manager_utils:get_curr_shift_time/2).

make_shift_db_name(GetShiftTimeFun) ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrame = proplists:get_value(shift_frame, DynamicProps),
	DbNameFmt = proplists:get_value(mongodb_dbname_fmt, DynamicProps),

	CurrTime = k_datetime:utc_datetime(),
	{{ShiftYear, ShiftMonth, ShiftDay}, _} = GetShiftTimeFun(CurrTime, ShiftFrame),

	%% build db name in format YYYY-MM-DD.
	ShiftDateStr = lists:flatten(io_lib:format("~B_~2..0B_~2..0B", [ShiftYear, ShiftMonth, ShiftDay])),
	ShiftDbName = list_to_binary(io_lib:format(DbNameFmt, [ShiftDateStr])),
	ShiftDbName.
