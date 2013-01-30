-module(k_storage_events_manager).

%% API
-export([
	start_link/0,
	get_storage_mode/0
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

-record(state, {
	heartbeat_timer_ref :: reference(),

	curr_shift_time :: calendar:datetime(),
	next_shift_time :: calendar:datetime(),

	storage_mode :: atom(),
	next_event :: any()
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
	gen_server:call(Pid, get_storage_mode).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% on startup fire on_start_event -> to start static storage
%% on startup fire set_mode_event

init([]) ->
	gproc:reg({n, l, ?MODULE}),

	CurrTime = k_datetime:utc_time(),

	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrequency = proplists:get_value(shift_frequency, DynamicProps),
	ResponseFrame = proplists:get_value(response_frame, DynamicProps),
	DeliveryFrame = proplists:get_value(delivery_frame, DynamicProps),

	CurrShiftTime = k_storage_events_utils:get_curr_shift_time(CurrTime, ShiftFrequency),
	NextShiftTime = k_storage_events_utils:get_next_shift_time(CurrTime, ShiftFrequency),

	CurrMode = get_storage_mode(CurrTime, CurrShiftTime, ResponseFrame, DeliveryFrame),
	NextEvent = get_next_event(CurrMode, CurrShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime),

	{ok, TimerRef} = start_heartbeat_timer(),

	{ok, #state{
		heartbeat_timer_ref = TimerRef,
		curr_shift_time = CurrShiftTime,
		next_shift_time = NextShiftTime,
		storage_mode = CurrMode,
		next_event = NextEvent
	}}.

handle_call(get_storage_mode, _From, State = #state{
	storage_mode = CurrMode
}) ->
	{reply, {ok, CurrMode}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({timeout, TimerRef, {heartbeat}}, State = #state{
	heartbeat_timer_ref = TimerRef,
	curr_shift_time = CurrShiftTime,
	next_shift_time = NextShiftTime,
	storage_mode = CurrMode,
	next_event = {CurrEvent, CurrEventTime}
}) ->
   	%?log_debug("~p", [State]),

	CurrTime = k_datetime:utc_time(),

	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrequency = proplists:get_value(shift_frequency, DynamicProps),
	ResponseFrame = proplists:get_value(response_frame, DynamicProps),
	DeliveryFrame = proplists:get_value(delivery_frame, DynamicProps),

	NewState =
		case CurrTime >= CurrEventTime of
			true ->
				NextMode = get_next_mode(CurrMode, CurrEvent),
				case CurrEvent of
					'ShiftEvent' ->
						NewNextShiftTime = k_storage_events_utils:get_next_shift_time(CurrTime, ShiftFrequency),
						NextEvent = get_next_event(
							NextMode, NextShiftTime, ResponseFrame, DeliveryFrame, NewNextShiftTime
						),
						?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
						k_storage_manager:notify_event({CurrMode, CurrEvent, NextMode}),
						State#state{
							curr_shift_time = NextShiftTime,
							next_shift_time = NewNextShiftTime,
							storage_mode = NextMode,
							next_event = NextEvent
						};
					_ ->
						NextEvent = get_next_event(
							NextMode, CurrShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime
						),
						?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
						k_storage_manager:notify_event({CurrMode, CurrEvent, NextMode}),
						State#state{
							storage_mode = NextMode,
							next_event = NextEvent
						}
				end;
			false ->
				State
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

get_storage_mode(CurrTime, CurrShiftTime, ResponseFrame, DeliveryFrame) ->
	CurrShiftSecs = calendar:datetime_to_gregorian_seconds(CurrShiftTime),
	CurrTimeSecs = calendar:datetime_to_gregorian_seconds(CurrTime),
	ResponseFrameSecs = k_storage_events_utils:to_seconds(ResponseFrame),
	DeliveryFrameSecs = k_storage_events_utils:to_seconds(DeliveryFrame),

	DiffSecs = CurrTimeSecs - CurrShiftSecs,
	if
		DiffSecs < ResponseFrameSecs ->
			'Response';
		DiffSecs < DeliveryFrameSecs ->
			'Delivery';
		true ->
			'Normal'
	end.

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
