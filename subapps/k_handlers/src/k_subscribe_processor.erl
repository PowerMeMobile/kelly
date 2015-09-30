-module(k_subscribe_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(term()) -> {ok, term()} | {error, term()}.
process(Req = #sub_sms_receipts_req_v1{}) ->
    #sub_sms_receipts_req_v1{
        req_id = ReqId,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        url = NotifyURL,
        dest_addr = SrcAddr, %% TODO: HERE MUST BE source_addr
        callback_data = CallbackData
    } = Req,
    %% TODO: Ensure correct SrcAddr for Customer:User
    {ok, QName} = application:get_env(k_handlers, oneapi_receipt_sms_queue),
    Sub = #k_mb_oneapi_receipt_sub{
        id = ReqId,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        queue_name = QName,
        src_addr = SrcAddr,
        notify_url = NotifyURL,
        callback_data = CallbackData,
        created_at = ac_datetime:utc_timestamp()
    },
    k_mailbox:register_subscription(Sub),
    Resp = #sub_sms_receipts_resp_v1{
        req_id = ReqId
    },
    {ok, Resp};
process(Req = #unsub_sms_receipts_req_v1{}) ->
    #unsub_sms_receipts_req_v1{
        req_id = ReqId,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        subscription_id = SubId
    } = Req,
    ok = k_mailbox:unregister_subscription(SubId, CustomerUuid, UserId),
    Resp = #unsub_sms_receipts_resp_v1{
        req_id = ReqId
    },
    {ok, Resp};
process(Req = #sub_incoming_sms_req_v1{}) ->
    #sub_incoming_sms_req_v1{
        req_id = Id,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        dest_addr = DstAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        correlator = _Correlator,
        callback_data = CallbackData
    } = Req,
    %% TODO: Ensure correct DstAddr for Customer:User
    {ok, QName} = application:get_env(k_handlers, oneapi_incoming_sms_queue),
    Sub = #k_mb_oneapi_incoming_sub{
        id = Id,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        priority = 0,
        queue_name = QName,
        dst_addr = DstAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        callback_data = CallbackData,
        created_at = ac_datetime:utc_timestamp()
    },
    k_mailbox:register_subscription(Sub),
    Resp = #sub_incoming_sms_resp_v1{
        req_id = Id,
        subscription_id = Id
    },
    {ok, Resp};
process(Req = #unsub_incoming_sms_req_v1{}) ->
    #unsub_incoming_sms_req_v1{
        req_id = ReqId,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        subscription_id = SubId
    } = Req,
    ok = k_mailbox:unregister_subscription(SubId, CustomerUuid, UserId),
    Resp = #unsub_incoming_sms_resp_v1{
        req_id = ReqId
    },
    {ok, Resp}.
