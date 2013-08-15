-module(k_storage_manager).

%% API
-export([
	get_storage_mode/0
]).

%% gen_storage_manager callbacks
-export([
	ensure_static_storage_indexes/1,
	ensure_dynamic_storage_indexes/1,
	next_mode_event/5,
	new_state/1,
	decode_state/1,
	encode_state/1
]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").

-type event_name() :: gen_storage_manager:event_name() | 'ResponseEndEvent' | 'DeliveryEndEvent'.
-type storage_mode() :: gen_storage_manager:storage_mode() | 'ResponseMode' | 'DeliveryMode'.
-type reason() :: term().

-record(state, {
	response_frame,
	delivery_frame
}).

%% ===================================================================
%% API
%% ===================================================================

-spec get_storage_mode() -> {ok, storage_mode()} | {error, reason()}.
get_storage_mode() ->
	case gen_storage_manager:get_storage_mode() of
		{ok, StorageMode} ->
			Mode =
				case StorageMode of
					'ResponseMode' -> k_response_mode_storage;
					'DeliveryMode' -> k_delivery_mode_storage;
					'RegularMode'   -> k_normal_mode_storage
				end,
			{ok, Mode};
		Error ->
			Error
	end.

%% ===================================================================
%% gen_storage_manager callbacks
%% ===================================================================

ensure_static_storage_indexes(ServerName) ->
	ok = mongodb_storage:ensure_index(ServerName, k1api_sms_request_id_to_msg_ids,
		{key, {customer_id, 1, user_id, 1, src_addr, 1, req_id, 1}}).

ensure_dynamic_storage_indexes(ServerName) ->
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {ri, 1, imi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {ci, 1, ui, 1, ct, 1, imi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {gi, 1, omi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {rqt, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mo_messages,
		{key, {rqt, 1}}).

new_state(Props) ->
	ResponseFrame = proplists:get_value(response_frame, Props),
	DeliveryFrame = proplists:get_value(delivery_frame, Props),
	#state{
		response_frame = ResponseFrame,
		delivery_frame = DeliveryFrame
	}.

decode_state(Doc) ->
	ResponseFrame = bsondoc:at(response_frame, Doc),
	DeliveryFrame = bsondoc:at(delivery_frame, Doc),
	#state{
		response_frame = ResponseFrame,
		delivery_frame = DeliveryFrame
	}.

encode_state(#state{
	response_frame = ResponseFrame,
	delivery_frame = DeliveryFrame
}) ->
	{
		response_frame , ResponseFrame,
		delivery_frame , DeliveryFrame
	}.

next_mode_event(Mode, Event, CurrShiftTime, NextShiftTime, State) ->
	NextMode = get_next_mode(Mode, Event),
	{NextEvent, NextEventTime, NextState} = get_next_event(
		NextMode, CurrShiftTime, NextShiftTime, State
	),
	{NextMode, NextEvent, NextEventTime, NextState}.

%% ===================================================================
%% Internal
%% ===================================================================

get_next_mode(undefined, undefined) ->
	'RegularMode';
get_next_mode('RegularMode', 'ShiftEvent') ->
	'ResponseMode';
get_next_mode('ResponseMode', 'ResponseEndEvent') ->
	'DeliveryMode';
get_next_mode('DeliveryMode', 'DeliveryEndEvent') ->
	'RegularMode'.

get_next_event('RegularMode', _CurrShiftTime, NextShiftTime, State) ->
	{'ShiftEvent', NextShiftTime, State};
get_next_event('ResponseMode', CurrShiftTime, _NextShiftTime, State = #state{
	response_frame = ResponseFrame
}) ->
	EventTime = gen_storage_manager_utils:add_frame(CurrShiftTime, ResponseFrame),
	{'ResponseEndEvent', EventTime, State};
get_next_event('DeliveryMode', CurrShiftTime, _NextShiftTime, State = #state{
	delivery_frame = DeliveryFrame
}) ->
	EventTime = gen_storage_manager_utils:add_frame(CurrShiftTime, DeliveryFrame),
	{'DeliveryEndEvent', EventTime, State}.
