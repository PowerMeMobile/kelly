-module(k_storage_events_manager).

%% API
-export([
	start_link/0
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

-record(state, {
	timer_ref :: reference(),

	prev_shift_time :: calendar:datetime(),
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

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% on startup, also fire on_start_event -> to start static storage

init([]) ->
	CurrTime = get_curr_time(),

	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	ShiftFrequency = proplists:get_value(shift_frequency, DynamicProps),
	ResponseFrame = proplists:get_value(response_frame, DynamicProps),
	DeliveryFrame = proplists:get_value(delivery_frame, DynamicProps),

	PrevShiftTime = k_storage_events_utils:get_prev_shift_time(CurrTime, ShiftFrequency),
	NextShiftTime = k_storage_events_utils:get_next_shift_time(CurrTime, ShiftFrequency),

	CurrMode = get_storage_mode(CurrTime, PrevShiftTime, ResponseFrame, DeliveryFrame),
	NextEvent = get_next_event(CurrMode, PrevShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime),

	{ok, TimerRef} = start_timer(),

	{ok, #state{
		timer_ref = TimerRef,
		prev_shift_time = PrevShiftTime,
		next_shift_time = NextShiftTime,
		storage_mode = CurrMode,
		next_event = NextEvent
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({timeout, TimerRef, {}}, State = #state{
	timer_ref = TimerRef,
	prev_shift_time = PrevShiftTime,
	next_shift_time = NextShiftTime,
	storage_mode = CurrMode,
	next_event = {CurrEvent, CurrEventTime}
}) ->
   	%?log_debug("~p", [State]),

	CurrTime = get_curr_time(),

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
						NextEvent = get_next_event(NextMode, NextShiftTime, ResponseFrame, DeliveryFrame, NewNextShiftTime),
						?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
						State#state{
							prev_shift_time = NextShiftTime,
							next_shift_time = NewNextShiftTime,
							storage_mode = NextMode,
							next_event = NextEvent
						};
					_ ->
						NextEvent = get_next_event(NextMode, PrevShiftTime, ResponseFrame, DeliveryFrame, NextShiftTime),
						?log_info("~p -> ~p -> ~p ~p", [CurrMode, CurrEvent, NextMode, NextEvent]),
						State#state{
							storage_mode = NextMode,
							next_event = NextEvent
						}
				end;
			false ->
				State
		end,

	{ok, NewTimerRef} = start_timer(),

	{noreply, NewState#state{timer_ref = NewTimerRef}};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

start_timer() ->
	TimerRef = erlang:start_timer(1000, self(), {}),
	{ok, TimerRef}.

get_storage_mode(CurrTime, PrevShiftTime, ResponseFrame, DeliveryFrame) ->
	PrevShiftSecs = calendar:datetime_to_gregorian_seconds(PrevShiftTime),
	CurrTimeSecs = calendar:datetime_to_gregorian_seconds(CurrTime),
	ResponseFrameSecs = k_storage_events_utils:to_seconds(ResponseFrame),
	DeliveryFrameSecs = k_storage_events_utils:to_seconds(DeliveryFrame),

	DiffSecs = CurrTimeSecs - PrevShiftSecs,
	if
		DiffSecs < ResponseFrameSecs ->
			'Response';
		DiffSecs < DeliveryFrameSecs ->
			'Delivery';
		true ->
			'Normal'
	end.

get_next_event('Normal', _PrevShiftTime, _ResponseFrame, _DeliveryFrame, NextShiftTime) ->
	{'ShiftEvent', NextShiftTime};
get_next_event('Response', PrevShiftTime, ResponseFrame, _DeliveryFrame, _NextShiftTime) ->
	EventTime = k_storage_events_utils:add_frame(PrevShiftTime, ResponseFrame),
	{'ResponseEndEvent', EventTime};
get_next_event('Delivery', PrevShiftTime, _ResponseFrame, DeliveryFrame, _NextShiftTime) ->
	EventTime = k_storage_events_utils:add_frame(PrevShiftTime, DeliveryFrame),
	{'DeliveryEndEvent', EventTime}.

get_next_mode('Normal', 'ShiftEvent') ->
	'Response';
get_next_mode('Response', 'ResponseEndEvent') ->
	'Delivery';
get_next_mode('Delivery', 'DeliveryEndEvent') ->
	'Normal'.

%% ===================================================================
%% Redesign
%% ===================================================================

get_curr_time() ->
	k_time_server:get_utc_time().
	%calendar:universal_time().
