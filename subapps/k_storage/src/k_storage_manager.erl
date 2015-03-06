-module(k_storage_manager).

-behaviour(gen_storage_manager).

%% API
-export([
    get_storage_mode/0
]).

%% gen_storage_manager callbacks
-export([
    ensure_static_storage_indexes/1,
    ensure_mailbox_storage_indexes/1,
    ensure_defers_storage_indexes/1,
    ensure_dynamic_storage_indexes/1,

    next_mode_event/5,
    new_spec_state/1,
    decode_spec_state/1,
    encode_spec_state/1
]).

-include("application.hrl").
-include_lib("alley_common/include/logging.hrl").

-type event_name() :: gen_storage_manager:event_name() | 'ResponseEndEvent' | 'DeliveryEndEvent'.
-type storage_mode() :: gen_storage_manager:storage_mode() | 'ResponseMode' | 'DeliveryMode'.
-type server_name() :: mondodb_storage:server_name().
-type plist() :: [{atom(), term()}].
-type reason() :: term().

-record(state, {
    response_frame,
    delivery_frame
}).
-type spec_state() :: #state{}.

%% ===================================================================
%% API
%% ===================================================================

-spec get_storage_mode() -> {ok, storage_mode()} | {error, reason()}.
get_storage_mode() ->
    case gen_storage_manager:get_storage_mode() of
        {ok, StorageMode} ->
            Mode =
                case StorageMode of
                    'RegularMode'  -> k_storage_regular_mode;
                    'ResponseMode' -> k_storage_response_mode;
                    'DeliveryMode' -> k_storage_delivery_mode
                end,
            {ok, Mode};
        Error ->
            Error
    end.

%% ===================================================================
%% gen_storage_manager callbacks
%% ===================================================================

-spec ensure_static_storage_indexes(server_name()) -> ok.
ensure_static_storage_indexes(ServerName) ->
    ok = mongodb_storage:ensure_index(ServerName, customers,
        {key, {customer_id, 1}}).

-spec ensure_mailbox_storage_indexes(server_name()) -> ok.
ensure_mailbox_storage_indexes(_ServerName) ->
    ok.

-spec ensure_defers_storage_indexes(server_name()) -> ok.
ensure_defers_storage_indexes(_ServerName) ->
    ok.

-spec ensure_dynamic_storage_indexes(server_name()) -> ok.
ensure_dynamic_storage_indexes(ServerName) ->
    ok = mongodb_storage:ensure_index(ServerName, mt_messages,
        {key, {ri, 1, imi, 1}}),
    ok = mongodb_storage:ensure_index(ServerName, mt_messages,
        {key, {ci, 1, ui, 1, ct, 1, imi, 1}}),
    ok = mongodb_storage:ensure_index(ServerName, mt_messages,
        {key, {gi, 1, omi, 1, rqt, -1}}),
    ok = mongodb_storage:ensure_index(ServerName, mt_messages,
        {key, {rqt, 1, ci, 1}}),
    ok = mongodb_storage:ensure_index(ServerName, mo_messages,
        {key, {rqt, 1}}),
    ok = mongodb_storage:ensure_index(ServerName, mo_messages,
        {key, {ci, 1, ui, 1, s, 1}}).

-spec new_spec_state(plist()) -> spec_state().
new_spec_state(Props) ->
    ResponseFrame = proplists:get_value(response_frame, Props),
    DeliveryFrame = proplists:get_value(delivery_frame, Props),
    #state{
        response_frame = ResponseFrame,
        delivery_frame = DeliveryFrame
    }.

-spec decode_spec_state(bson:document()) -> spec_state().
decode_spec_state(Doc) ->
    ResponseFrame = bsondoc:at(response_frame, Doc),
    DeliveryFrame = bsondoc:at(delivery_frame, Doc),
    #state{
        response_frame = ResponseFrame,
        delivery_frame = DeliveryFrame
    }.

-spec encode_spec_state(spec_state()) -> bson:document().
encode_spec_state(#state{
    response_frame = ResponseFrame,
    delivery_frame = DeliveryFrame
}) ->
    {
        response_frame , ResponseFrame,
        delivery_frame , DeliveryFrame
    }.

-spec next_mode_event(
    storage_mode(), event_name(), calendar:datetime(), calendar:datetime(), spec_state()
) ->
     {storage_mode(), event_name(), calendar:datetime(), spec_state()}.
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
