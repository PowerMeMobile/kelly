-module(k_subscribe_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(term()) -> {ok, term()} | {error, term()}.
process(Request = #k1api_subscribe_incoming_sms_request_dto{}) ->
    #k1api_subscribe_incoming_sms_request_dto{
        id = Id,
        customer_id = CustomerId,
        user_id = _UserId,
        dest_addr = DestAddr,
        notify_url = URL,
        criteria = Criteria,
        correlator = _Correlator,
        callback_data = Callback
    } = Request,
    {ok, QName} = application:get_env(k_handlers, oneapi_incoming_sms_queue),
    Subscription = #k_mb_k1api_incoming_sms_sub{
        id = Id,
        customer_id = CustomerId,
        user_id = <<"undefined">>,
        priority = 0,
        queue_name = QName,
        dest_addr = DestAddr,
        notify_url = URL,
        criteria = Criteria,
        callback_data = Callback,
        created_at = ac_datetime:utc_timestamp()
    },
    k_mailbox:register_subscription(Subscription),
    Response = #k1api_subscribe_incoming_sms_response_dto{
        id = Id,
        subscription_id = Id
    },
    {ok, Response};
process(Request = #k1api_unsubscribe_incoming_sms_request_dto{}) ->
    #k1api_unsubscribe_incoming_sms_request_dto{
        id = RequestId,
        customer_id = CustomerId,
        user_id = _UserId,
        subscription_id = SubscriptionId
    } = Request,
    UserId = <<"undefined">>,
    ok = k_mailbox:unregister_subscription(SubscriptionId, CustomerId, UserId),
    Response = #k1api_unsubscribe_incoming_sms_response_dto{
        id = RequestId
    },
    {ok, Response};
process(Request = #k1api_subscribe_sms_receipts_request_dto{}) ->
    #k1api_subscribe_sms_receipts_request_dto{
        id = Id,
        customer_id = CustomerId,
        user_id = _UserId,
        url = URL,
        dest_addr = DestAddr,
        callback_data = Callback
    } = Request,
    {ok, QName} = application:get_env(k_handlers, oneapi_receipt_sms_queue),
    Subscription = #k_mb_k1api_receipt_sub{
        id = Id,
        customer_id = CustomerId,
        user_id = <<"undefined">>,
        queue_name = QName,
        dest_addr = DestAddr,
        notify_url = URL,
        callback_data = Callback,
        created_at = ac_datetime:utc_timestamp()
    },
    k_mailbox:register_subscription(Subscription),
    Response = #k1api_subscribe_sms_receipts_response_dto{
        id = Id
    },
    {ok, Response};
process(Request = #k1api_unsubscribe_sms_receipts_request_dto{}) ->
    #k1api_unsubscribe_sms_receipts_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = _UserId,
        subscription_id = SubscriptionId
    } = Request,
    UserId = <<"undefined">>,
    ok = k_mailbox:unregister_subscription(SubscriptionId, CustomerId, UserId),
    Response = #k1api_unsubscribe_sms_receipts_response_dto{
        id = ReqId
    },
    {ok, Response}.
