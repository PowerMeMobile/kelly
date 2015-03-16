-module(k_receipt_batch_handler).

-export([process/1]).
-export([register_delivery_receipt/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").
-include_lib("k_storage/include/msg_info.hrl").

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
process(<<"ReceiptBatch">>, ReqBin) ->
    case adto:decode(#just_delivery_receipt_dto{}, ReqBin) of
        {ok, ReceiptBatch} ->
            ?log_debug("Got receipt batch: ~p", [ReceiptBatch]),
            %% it's quite possible that req/resp aren't handled yet or
            %% the receipt can't be handled at all. don't waste resources
            %% trying to requeue multiple times, because it doesn't work
            %% if a wrong receipt is received. at the same time, under load
            %% it's possible that req/resp stored into db after some time.
            %% wait and try a number of times, and if it fails, then skip it.
            {ok, Timeouts} =
                application:get_env(k_handlers, receipt_retry_timeouts),
            case process_batch_with_timeouts(ReceiptBatch, Timeouts) of
                ok ->
                    {ok, []};
                {error, no_entry} ->
                    ?log_warn("Bad receipt batch: ~p", [ReceiptBatch]),
                    {ok, []};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
process(ReqCT, ReqBin) ->
    ?log_error("Got unknown receipt batch ~p: ~p", [ReqCT, ReqBin]),
    {ok, []}.

process_batch_with_timeouts(_ReceiptBatch, []) ->
    {error, no_entry};
process_batch_with_timeouts(ReceiptBatch, [Timeout | Timeouts]) ->
    ?log_debug("Waiting ~p ms before processing receipt batch: ~p",
        [Timeout, ReceiptBatch]),
    timer:sleep(Timeout),

    case process_batch(ReceiptBatch) of
        ok ->
            ?log_debug("Processed receipt batch: ~p", [ReceiptBatch]),
            ok;
        {error, no_entry} ->
            ?log_debug("Not enough data to receipt batch: ~p",
                [ReceiptBatch]),
            process_batch_with_timeouts(ReceiptBatch, Timeouts)
    end.

process_batch(ReceiptBatch) ->
    GatewayId = ReceiptBatch#just_delivery_receipt_dto.gateway_id,
    Receipts = ReceiptBatch#just_delivery_receipt_dto.receipts,
    UTCString = ReceiptBatch#just_delivery_receipt_dto.timestamp,
    DlrTime = ac_datetime:utc_string_to_timestamp(UTCString),
    traverse_receipts(GatewayId, DlrTime, Receipts).

traverse_receipts(_GatewayId, _DlrTime, []) ->
    ok;
traverse_receipts(GatewayId, DlrTime, [Receipt | Receipts]) ->
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
            ok = register_delivery_receipt(MsgInfo),
            traverse_receipts(GatewayId, DlrTime, Receipts);
        Error ->
            Error
    end.

%% The function is called from k_sms_response_handler
%% if a submit error is detected and a delivery receipt is requested.
-spec register_delivery_receipt(#msg_info{}) -> ok.
register_delivery_receipt(#msg_info{client_type = ClientType})
    when ClientType =:= mm orelse ClientType =:= soap ->
    %% no need to register receipts.
    ok;
register_delivery_receipt(MsgInfo) ->
    Item = build_receipt_item(MsgInfo),
    ok = k_mailbox:register_incoming_item(Item).

build_receipt_item(#msg_info{
    client_type = oneapi,
    customer_uuid = CustomerUuid,
    user_id = UserId,
    req_id = ReqId,
    in_msg_id = InMsgId,
    src_addr = SrcAddr,
    dst_addr = DstAddr,
    status = Status,
    req_time = ReqTime,
    dlr_time = DlrTime
}) ->
    ItemId = uuid:unparse(uuid:generate_time()),
    #k_mb_k1api_receipt{
        id = ItemId,
        customer_id = CustomerUuid,
        user_id = UserId,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        req_id = ReqId,
        in_msg_id = InMsgId,
        submit_date = ReqTime,
        done_date = DlrTime,
        status = Status
    };
build_receipt_item(#msg_info{
    client_type = funnel,
    customer_uuid = CustomerUuid,
    user_id = UserId,
    req_id = ReqId,
    in_msg_id = InMsgId,
    src_addr = SrcAddr,
    dst_addr = DstAddr,
    status = Status,
    req_time = ReqTime,
    dlr_time = DlrTime
}) ->
    ItemId = uuid:unparse(uuid:generate_time()),
    #k_mb_funnel_receipt{
        id = ItemId,
        customer_id = CustomerUuid,
        user_id = UserId,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        req_id = ReqId,
        in_msg_id = InMsgId,
        submit_date = ReqTime,
        done_date = DlrTime,
        status = Status
    }.
