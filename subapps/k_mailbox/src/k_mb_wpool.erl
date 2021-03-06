-module(k_mb_wpool).

-behaviour(gen_wp).

-include_lib("k_storage/include/mailbox.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_wp/include/gen_wp_spec.hrl").

%% API
-export([
    start_link/0,
    process_incoming_item/1
]).

%% GenWp Callback
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2,

    handle_fork_cast/3,
    handle_fork_call/4,
    handle_child_forked/3,
    handle_child_terminated/4
]).

-record(state, {
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    WPSize = k_mb_config:get_env(pool_size),
    gen_wp:start_link({local, ?MODULE}, ?MODULE, [], [{pool_size, WPSize}]).

-spec process_incoming_item(k_mb_item() | item_id()) -> ok.
process_incoming_item(Item) ->
    gen_wp:cast(?MODULE, {process_item, Item}).

%% ===================================================================
%% GenWorkerPool Callbacks
%% ===================================================================

init([]) ->
    ?log_debug("Started", []),
    {ok, ItemSets} = k_storage_mailbox:get_items(),
    Process =
        fun({ItemType, ItemIDs}) ->
            lists:foreach(
                fun(ItemID) ->
                    process_incoming_item({ItemType, ItemID})
                end,
                ItemIDs
            )
        end,
    lists:foreach(Process, ItemSets),
    {ok, #state{}}.

handle_call(Request, _ReplyTo, State) ->
    {stop, {badarg, Request}, badarg, State}.

handle_cast({process_item, Item}, State = #state{}) ->
    {fork, {process_item, Item}, State};

handle_cast(Message, State) ->
    {stop, {badarg, Message}, State}.

handle_info(Info, State) ->
    {stop, {badarg, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_fork_call( _Arg, _CallMess, _ReplyTo, _WP ) ->
    {noreply, bad_request}.

handle_fork_cast(_Arg, {process_item, {ItemType, ItemID}}, _WP) when
    ItemType == k_mb_incoming orelse
    ItemType == k_mb_oneapi_receipt orelse
    ItemType == k_mb_funnel_receipt ->

    case k_storage_mailbox:get_item(ItemType, ItemID) of
        {ok, Item} -> process(Item);
        no_record -> ok
    end,
    {noreply, normal};
handle_fork_cast(_Arg, {process_item, Item}, _WP) ->
    process(Item),
    {noreply, normal};

handle_fork_cast(_Arg, _CastMess, _WP) ->
    {noreply, bad_request}.

handle_child_forked(_Task, _Child, State = #state{}) ->
    {noreply, State#state{}}.

handle_child_terminated(normal, _Task, _Child, State = #state{}) ->
    {noreply, State#state{}};

handle_child_terminated(Reason, _Task = {process, Item}, _Child, State = #state{}) ->
    case k_mb_postponed_queue:postpone(Item) of
        {postponed, Seconds} ->
            ?log_error("Item processing terminated. It will be resend "
                "after ~p sec. Reason: ~p", [Seconds, Reason]),
            {noreply, State#state{}};
        {error, reached_max} ->
            ?log_error("Item reached max number of attempts. "
                "Discard message. Reason: ~p", [Reason]),
            {noreply, State#state{}};
        Error ->
            ?log_error("Got unexpected error: ~p. Terminating.", [Error]),
            {stop, {unexpected_case_clause, Error}}
    end.

%% ===================================================================
%% Local
%% ===================================================================

process(Item) ->
    case k_mb_subscription_mgr:get_suitable_subscription(Item) of
        {ok, Subscription} ->
            ?log_debug("Found suitable subscription: ~p", [Subscription]),
            send_item(Item, Subscription);
        undefined ->
            ?log_debug("Suitable subscription NOT FOUND", []),
            ok
    end.

send_item(Item, Subscription) ->
    {ok, ItemID, QName, Binary} = build_dto(Item, Subscription),
    ?log_debug("Send item: ~p to queue: ~p", [ItemID, QName]),
    ContentType =
        case Item of
            #k_mb_incoming{}       -> <<"OutgoingBatch">>;
            #k_mb_oneapi_receipt{} -> <<"ReceiptBatch">>;
            #k_mb_funnel_receipt{} -> <<"ReceiptBatch">>
        end,
    case k_mb_amqp_producer_srv:send(ItemID, Binary, QName, ContentType) of
        {ok, delivered} ->
            Timestamp = ac_datetime:utc_timestamp(),
            k_storage_mailbox:save_delivery_status(Item, delivered, Timestamp),
            k_storage_mailbox:delete_item(Item),
            ?log_debug("Item successfully delivered: ~p", [ItemID]);
        {error, timeout} ->
            postpone_item(Item, timeout)
    end.

postpone_item(Item, Error) ->
    case k_mb_postponed_queue:postpone(Item) of
        {postponed, Seconds} ->
            ?log_error("Item processing terminated. It will be resend "
                "after ~p sec. Reason: ~p", [Seconds, Error]);
        {error, reached_max} ->
            ?log_error("Item reached max number of attempts. "
                "Discard message. Reason: ~p", [Error]),
            Timestamp = ac_datetime:utc_timestamp(),
            k_storage_mailbox:save_delivery_status(Item, reached_max, Timestamp)
    end.

build_dto(Item = #k_mb_funnel_receipt{}, Sub = #k_mb_funnel_sub{}) ->
    #k_mb_funnel_receipt{
        id = ItemID,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        in_msg_id = InMsgId,
        submit_date = SubmitDate,
        done_date = DoneDate,
        status = Status
     } = Item,
    #k_mb_funnel_sub{
        queue_name = QName
    } = Sub,
    Receipt = #funnel_delivery_receipt_container_dto{
        message_id = InMsgId,
        submit_date = ac_datetime:timestamp_to_utc_string(SubmitDate),
        done_date = ac_datetime:timestamp_to_utc_string(DoneDate),
        %% it's atom here, see #k_mb_oneapi_receipt{} case
        message_state = Status,
        source = SrcAddr,
        dest = DstAddr
    },
    DTO = #funnel_delivery_receipt_dto{
        id = ItemID,
        receipts = [Receipt]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, ItemID, QName, Bin};
build_dto(Item = #k_mb_incoming{}, Sub = #k_mb_funnel_sub{}) ->
    #k_mb_incoming{
        id = ItemID,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        body = Body,
        encoding = Encoding
    } = Item,
    #k_mb_funnel_sub{
        queue_name = QName
    } = Sub,
    Msg = #funnel_incoming_sms_message_dto{
        source = SrcAddr,
        dest = DstAddr,
        data_coding = Encoding,
        message = Body
    },
    Batch = #funnel_incoming_sms_dto{
        id = ItemID,
        messages = [Msg]
    },
    {ok, Bin} = adto:encode(Batch),
    {ok, ItemID, QName, Bin};
build_dto(Item = #k_mb_incoming{}, Sub = #k_mb_oneapi_incoming_sub{}) ->
    #k_mb_incoming{
        id = ItemID,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        rcv_time = RcvTime,
        body = Body
    } = Item,
    #k_mb_oneapi_incoming_sub{
        queue_name = QName,
        notify_url = NotifyURL,
        callback_data = CallbackData
    } = Sub,
    DTO = #incoming_sms_notification_v1{
        callback_data = CallbackData,
        datetime = RcvTime,
        dst_addr = DstAddr,
        message_id = ItemID,
        message = Body,
        sender_addr = SrcAddr,
        notify_url = NotifyURL
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, ItemID, QName, Bin};
build_dto(Item = #k_mb_oneapi_receipt{}, Sub = #k_mb_oneapi_receipt_sub{}) ->
    #k_mb_oneapi_receipt{
        id = ItemID,
        dst_addr = DstAddr,
        status = Status
    } = Item,
    #k_mb_oneapi_receipt_sub{
        queue_name = QName,
        callback_data = CallbackData,
        notify_url = NotifyURL
    } = Sub,
    DTO = #sms_receipt_notification_v1{
        id = ItemID,
        dst_addr = DstAddr,
        %% it's binary here, see #k_mb_funnel_receipt{} case
        status = atom_to_binary(Status, utf8),
        callback_data = CallbackData,
        url = NotifyURL
     },
    {ok, Bin} = adto:encode(DTO),
    {ok, ItemID, QName, Bin}.
