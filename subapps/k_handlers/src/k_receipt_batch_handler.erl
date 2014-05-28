-module(k_receipt_batch_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) ->
    {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(ContentType, Payload).

%% ===================================================================
%% Internals
%% ===================================================================

-spec process(binary(), binary()) ->
    {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ReceiptBatch">>, Message) ->
    case adto:decode(#just_delivery_receipt_dto{}, Message) of
        {ok, ReceiptBatch} ->
            process_receipt_batch(ReceiptBatch);
        Error ->
            Error
    end;

process(ContentType, Message) ->
    ?log_warn("Got unexpected message of type ~p: ~p", [ContentType, Message]),
    {ok, []}.

-spec process_receipt_batch(#just_delivery_receipt_dto{}) ->
    {ok, [#worker_reply{}]} | {error, any()}.
process_receipt_batch(ReceiptBatch) ->
    ?log_debug("Got delivery receipt: ~p", [ReceiptBatch]),
    GatewayId = ReceiptBatch#just_delivery_receipt_dto.gateway_id,
    Receipts = ReceiptBatch#just_delivery_receipt_dto.receipts,
    UTCString = ReceiptBatch#just_delivery_receipt_dto.timestamp,
    DlrTime = ac_datetime:utc_string_to_timestamp(UTCString),
    case traverse_delivery_receipts(GatewayId, DlrTime, Receipts) of
        ok ->
            {ok, []};
        %% abnormal case, sms response isn't handled yet.
        {error, no_entry} ->
            %% don't waste resources trying to requeue multiple times
            %% sleep for 10 secs before requeuing again.
            timer:sleep(10000),
            {error, not_enough_data_to_proceed};
        Error ->
            Error
    end.

traverse_delivery_receipts(_GatewayId, _DlrTime, []) ->
    ok;
traverse_delivery_receipts(GatewayId, DlrTime, [Receipt | Receipts]) ->
    OutMsgId = Receipt#just_receipt_dto.message_id,
    DlrStatus = Receipt#just_receipt_dto.message_state,
    %% note that this is one-shot call. it tries to store #dlr_info{}
    %% and, if succeeds, it returns the whole document.
    DlrInfo = #dlr_info{
        gateway_id = GatewayId,
        out_msg_id = OutMsgId,
        dlr_time = DlrTime,
        dlr_status = DlrStatus
    },
    case k_dynamic_storage:set_mt_dlr_info_and_get_msg_info(DlrInfo) of
        {ok, MsgInfo} ->
            ok = process_delivery_receipt(MsgInfo),
            %% process the rest receipts.
            traverse_delivery_receipts(GatewayId, DlrTime, Receipts);
        Error ->
            Error
    end.

process_delivery_receipt(MsgInfo) ->
    register_delivery_receipt(MsgInfo).

register_delivery_receipt(#msg_info{client_type = ClientType})
    when ClientType =:= mm orelse ClientType =:= soap ->
    %% no need to register receipts.
    ok;
register_delivery_receipt(MsgInfo) ->
    {ok, Item} = build_receipt_item(MsgInfo),
    ok = k_mailbox:register_incoming_item(Item).

build_receipt_item(#msg_info{
    client_type = oneapi,
    customer_id = CustomerId,
    user_id = UserId,
    in_msg_id = InMsgId,
    src_addr = SrcAddr,
    dst_addr = DstAddr,
    status = Status,
    dlr_time = _DlrTime
}) ->
    ItemId = uuid:unparse(uuid:generate_time()),
    Item = #k_mb_k1api_receipt{
        id = ItemId,
        customer_id = CustomerId,
        user_id = UserId,
        source_addr = SrcAddr,
        dest_addr = DstAddr,
        input_message_id = InMsgId,
        message_state = Status
    },
    {ok, Item};
build_receipt_item(#msg_info{
    client_type = funnel,
    customer_id = CustomerId,
    user_id = UserId,
    in_msg_id = InMsgId,
    src_addr = SrcAddr,
    dst_addr = DstAddr,
    status = Status,
    dlr_time = DlrTime
}) ->
    ItemId = uuid:unparse(uuid:generate_time()),
    Item = #k_mb_funnel_receipt{
        id = ItemId,
        customer_id = CustomerId,
        user_id = UserId,
        source_addr = SrcAddr,
        dest_addr = DstAddr,
        input_message_id = InMsgId,
        submit_date = DlrTime,
        done_date = DlrTime,
        message_state = Status
     },
    {ok, Item}.
