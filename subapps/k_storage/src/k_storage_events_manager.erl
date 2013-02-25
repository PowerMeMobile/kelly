-module(k_storage_events_manager).

%% API
-export([
	start_link/0,
	get_storage_mode/0,
	get_prev_shift_db_name/0,
	get_curr_shift_db_name/0
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

-type storage_mode() :: 'Response' | 'Delivery' | 'Normal'.
-type event_name() :: 'ResponseEndEvent' | 'DeliveryEndEvent' | 'ShiftEvent'.

-record(state, {
	heartbeat_timer_ref :: reference(),

	shift_frame,
	response_frame,
	delivery_frame,

	curr_shift_time :: calendar:datetime(),
	next_shift_time :: calendar:datetime(),
	shifts,

	curr_mode :: storage_mode(),
	next_event :: {event_name(), calendar:datetime()}
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_storage_mode() -> {ok, storage_mode()}.
get_storage_mode() ->
	{Pid, _} = gproc:await({n, l, ?MODULE}),
	gen_server:call(Pid, get_curr_mode).

-spec get_prev_shift_db_name() -> {ok, binary()}.
get_prev_shift_db_name() ->
	{Pid, _} = gproc:await({n, l, ?MODULE}),
	gen_server:call(Pid, get_prev_shift_db_name).

-spec get_curr_shift_db_name() -> {ok, binary()}.
get_curr_shift_db_name() ->
	{Pid, _} = gproc:await({n, l, ?MODULE}),
	gen_server:call(Pid, get_curr_shift_db_name).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	%% waiting for static storage to be ready...
	k_storage_manager:wait_for_static_storage(),

	CurrTime = k_datetime:utc_time(),

	{ok, NewState} =
		 case read_storage_state() of
			{ok, State} ->
				check_storage_state(CurrTime, State);
			{error, no_entry} ->
				make_storage_state(CurrTime)
		end,
	ok = write_storage_state(NewState),

	{ok, TimerRef} = start_heartbeat_timer(),

	%% signal events manager is ready.
	gproc:reg({n, l, ?MODULE}),
	{ok, NewState#state{heartbeat_timer_ref = TimerRef}}.

handle_call(get_curr_mode, _From, State = #state{
	curr_mode = CurrMode
}) ->
	{reply, {ok, CurrMode}, State};

handle_call(get_prev_shift_db_name, _From, State = #state{}) ->
	{ok, ShiftDbName} = prev_shift_db_name(),
	{reply, {ok, ShiftDbName}, State};

handle_call(get_curr_shift_db_name, _From, State = #state{}) ->
	{ok, ShiftDbName} = curr_shift_db_name(),
	{reply, {ok, ShiftDbName}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({timeout, TimerRef, {heartbeat}}, State = #state{
	heartbeat_timer_ref = TimerRef,
	next_event = {CurrEvent, CurrEventTime}
}) ->
   	%?log_debug("~p", [State]),

	CurrTime = k_datetime:utc_time(),

	{ok, NewState} =
		case CurrTime >= CurrEventTime of
			true ->
				process_event(CurrEvent, CurrTime, State);
			false ->
				{ok, State}
		end,

	{ok, NewTimerRef} = start_heartbeat_timer(),

	{noreply, NewState#state{
		heartbeat_timer_ref = NewTimerRef
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

start_heartbeat_timer() ->
	TimerRef = erlang:start_timer(1000, self(), {heartbeat}),
	{ok, TimerRef}.

read_storage_state() ->
	case mongodb_storage:find_one(k_static_storage, 'storage.state', {}) of
		{ok, Doc} ->
			ShiftFrame = bson:at(shift_frame, Doc),
			ResponseFrame = bson:at(response_frame, Doc),
			DeliveryFrame = bson:at(delivery_frame, Doc),
			CurrShiftTime = bson:at(curr_shift_time, Doc),
			NextShiftTime = bson:at(next_shift_time, Doc),
			Shifts = bson:at(shifts, Doc),
			CurrMode = bson:at(curr_mode, Doc),
			NextEvent = bson:at(next_event, Doc),
			NextEventTime = bson:at(next_event_time, Doc),
			State = #state{
				shift_frame = ShiftFrame,
				response_frame = ResponseFrame,
				delivery_frame = DeliveryFrame,
				curr_shift_time = k_datetime:timestamp_to_datetime(CurrShiftTime),
				next_shift_time = k_datetime:timestamp_to_datetime(NextShiftTime),
				shifts = Shifts,
				curr_mode = CurrMode,
				next_event = {NextEvent, k_datetime:timestamp_to_datetime(NextEventTime)}
			},
			{ok, State};
		Error ->
			Error
	end.

write_storage_state(#state{
	shift_frame = ShiftFrame,
	response_frame = ResponseFrame,
	delivery_frame = DeliveryFrame,
	curr_shift_time = CurrShiftTime,
	next_shift_time = NextShiftTime,
	shifts = Shifts,
	curr_mode = CurrMode,
	next_event = {NextEvent, NextEventTime}
}) ->
	Modifier = {
		shift_frame 	, ShiftFrame,
		response_frame  , ResponseFrame,
		delivery_frame  , DeliveryFrame,
		curr_shift_time , k_datetime:datetime_to_timestamp(CurrShiftTime),
		next_shift_time , k_datetime:datetime_to_timestamp(NextShiftTime),
		shifts          , Shifts,
		curr_mode       , CurrMode,
		next_event      , NextEvent,
		next_event_time , k_datetime:datetime_to_timestamp(NextEventTime)
	},
	ok = mongodb_storage:upsert(k_static_storage, 'storage.state', {}, Modifier),
	ok.

check_storage_state(_CurrTime, State = #state{
	shift_frame = _ShiftFrame,
	response_frame = _ResponseFrame,
	delivery_frame = _DeliveryFrame,

	curr_shift_time = _CurrShiftTime,
	next_shift_time = _NextShiftTime,
	shifts = _Shifts,

	curr_mode = _CurrMode,
	next_event = _NextEvent
}) ->
	%% put here code that is supposed to check/fix any storage state
	%% inconsistencies, caused by state and current time, etc.
	{ok, State}.

make_storage_state(CurrTime) ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrame = proplists:get_value(shift_frame, DynamicProps),
	ResponseFrame = proplists:get_value(response_frame, DynamicProps),
	DeliveryFrame = proplists:get_value(delivery_frame, DynamicProps),

	CurrShiftTime = k_storage_events_utils:get_curr_shift_time(CurrTime, ShiftFrame),
	NextShiftTime = k_storage_events_utils:get_next_shift_time(CurrTime, ShiftFrame),

	{ok, NewShiftDbName} = curr_shift_db_name(),

	%% if it's a clear start, then the current mode is 'Normal'.
	CurrMode = 'Normal',
	NextEvent = get_next_event(CurrMode, CurrShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime),

	{ok, #state{
		shift_frame = ShiftFrame,
		response_frame = ResponseFrame,
		delivery_frame = DeliveryFrame,

		curr_shift_time = CurrShiftTime,
		next_shift_time = NextShiftTime,
		shifts = [NewShiftDbName],

		curr_mode = CurrMode,
		next_event = NextEvent
	}}.

process_event(CurrEvent = 'ShiftEvent', CurrTime, State = #state{
	shift_frame = ShiftFrame,
	response_frame = ResponseFrame,
	delivery_frame = DeliveryFrame,
	next_shift_time = NextShiftTime,
	shifts = Shifts,
	curr_mode = CurrMode
}) ->
	NextMode = get_next_mode(CurrMode, CurrEvent),

	NewNextShiftTime = k_storage_events_utils:get_next_shift_time(CurrTime, ShiftFrame),
	NextEvent = get_next_event(
			NextMode, NextShiftTime, ResponseFrame, DeliveryFrame, NewNextShiftTime
	),
	?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
	ok = k_storage_manager:notify_event({CurrMode, CurrEvent, NextMode}),

	{ok, NewShiftDbName} = curr_shift_db_name(),

	NewState = State#state{
		curr_shift_time = NextShiftTime,
		next_shift_time = NewNextShiftTime,
		shifts = [NewShiftDbName | Shifts],
		curr_mode = NextMode,
		next_event = NextEvent
	},
	ok = write_storage_state(NewState),
	{ok, NewState};
process_event(CurrEvent, _CurrTime, State = #state{
	response_frame = ResponseFrame,
	delivery_frame = DeliveryFrame,
	next_shift_time = NextShiftTime,
	curr_shift_time = CurrShiftTime,
	curr_mode = CurrMode
}) when CurrEvent =:= 'ResponseEndEvent'; CurrEvent =:= 'DeliveryEndEvent' ->
	NextMode = get_next_mode(CurrMode, CurrEvent),

	NextEvent = get_next_event(
		NextMode, CurrShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime
	),
	?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
	ok = k_storage_manager:notify_event({CurrMode, CurrEvent, NextMode}),
	NewState = State#state{
		curr_mode = NextMode,
		next_event = NextEvent
	},
	ok = write_storage_state(NewState),
	{ok, NewState}.

prev_shift_db_name() ->
	make_shift_db_name(fun k_storage_events_utils:get_prev_shift_time/2).

curr_shift_db_name() ->
	make_shift_db_name(fun k_storage_events_utils:get_curr_shift_time/2).

make_shift_db_name(GetShiftTimeFun) ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrame = proplists:get_value(shift_frame, DynamicProps),
	DbNameFmt = proplists:get_value(mongodb_dbname_fmt, DynamicProps),

	CurrTime = k_datetime:utc_time(),
	{{ShiftYear, ShiftMonth, ShiftDay}, _} = GetShiftTimeFun(CurrTime, ShiftFrame),

	%% build db name in format YYYY-MM-DD.
	ShiftDateStr = lists:flatten(io_lib:format("~B_~2..0B_~2..0B", [ShiftYear, ShiftMonth, ShiftDay])),
	ShiftDbName = list_to_binary(io_lib:format(DbNameFmt, [ShiftDateStr])),
	{ok, ShiftDbName}.

get_next_event('Normal', _CurrShiftTime, _ResponseFrame, _DeliveryFrame, NextShiftTime) ->
	{'ShiftEvent', NextShiftTime};
get_next_event('Response', CurrShiftTime, ResponseFrame, _DeliveryFrame, _NextShiftTime) ->
	EventTime = k_storage_events_utils:add_frame(CurrShiftTime, ResponseFrame),
	{'ResponseEndEvent', EventTime};
get_next_event('Delivery', CurrShiftTime, _ResponseFrame, DeliveryFrame, _NextShiftTime) ->
	EventTime = k_storage_events_utils:add_frame(CurrShiftTime, DeliveryFrame),
	{'DeliveryEndEvent', EventTime}.

get_next_mode('Normal', 'ShiftEvent') ->
	'Response';
get_next_mode('Response', 'ResponseEndEvent') ->
	'Delivery';
get_next_mode('Delivery', 'DeliveryEndEvent') ->
	'Normal'.
