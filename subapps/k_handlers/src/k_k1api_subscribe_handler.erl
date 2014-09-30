-module(k_k1api_subscribe_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(Req, ContentType, Payload).

%% ===================================================================
%% API
%% ===================================================================

-spec process(term(), binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req, <<"SubscribeIncomingSms">>, Message) ->
    case adto:decode(#k1api_subscribe_incoming_sms_request_dto{}, Message) of
        {ok, SubRequest} ->
            ?log_debug("Got subscribe incoming sms request: ~p", [SubRequest]),
            process_subscription(Req, SubRequest);
        Error ->
            ?log_error("Subscribe incoming sms decode error: ~p", [Error]),
            {ok, []}
    end;
process(Req, <<"UnsubscribeIncomingSms">>, Message) ->
    case adto:decode(#k1api_unsubscribe_incoming_sms_request_dto{}, Message) of
        {ok, UnsubRequest} ->
            ?log_debug("Got unsubscribe incoming sms request: ~p", [UnsubRequest]),
            process_subscription(Req, UnsubRequest);
        Error ->
            ?log_error("Unsubscribe incoming sms decode error: ~p", [Error]),
            {ok, []}
    end;
process(Req, <<"SubscribeReceipts">>, Message) ->
    case adto:decode(#k1api_subscribe_sms_receipts_request_dto{}, Message) of
        {ok, SubRequest} ->
            ?log_debug("Got subscribe sms receipts request: ~p", [SubRequest]),
            process_subscription(Req, SubRequest);
        Error ->
            ?log_error("Subscribe sms receipts decode error: ~p", [Error]),
            {ok, []}
    end;
process(Req, <<"UnsubscribeReceipts">>, Message) ->
    case adto:decode(#k1api_unsubscribe_sms_receipts_request_dto{}, Message) of
        {ok, UnsubRequest} ->
            ?log_debug("Got unsubscribe sms receipts request: ~p", [UnsubRequest]),
            process_subscription(Req, UnsubRequest);
        Error ->
            ?log_error("Unsubscribe sms receipts decode error: ~p", [Error]),
            {ok, []}
    end;

process(_Req, ContentType, _Payload) ->
    ?log_warn("Got unexpected message type: ~p", [ContentType]),
    {ok, []}.

%% ===================================================================
%% Interal
%% ===================================================================

process_subscription(Req, SubRequest = #k1api_subscribe_incoming_sms_request_dto{}) ->
    #k1api_subscribe_incoming_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = _UserID,
        dest_addr = DestAddr,
        notify_url = URL,
        criteria = Criteria,
        correlator = _Correlator,
        callback_data = Callback
    } = SubRequest,
    {ok, QName} = application:get_env(k_handlers, oneapi_incoming_sms_queue),
    Subscription = #k_mb_k1api_incoming_sms_sub{
        id = ID,
        customer_id = CustomerID,
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
    ResponseDTO = #k1api_subscribe_incoming_sms_response_dto{
        id = ID,
        subscription_id = ID
    },
    step(is_reply_to_defined, Req, ResponseDTO);
process_subscription(Req, UnsubRequest = #k1api_unsubscribe_incoming_sms_request_dto{}) ->
    #k1api_unsubscribe_incoming_sms_request_dto{
        id = RequestID,
        customer_id = CustomerID,
        user_id = _UserID,
        subscription_id = SubscriptionID
    } = UnsubRequest,
    UserID = <<"undefined">>,
    ok = k_mailbox:unregister_subscription(SubscriptionID, CustomerID, UserID),
    ResponseDTO = #k1api_unsubscribe_incoming_sms_response_dto{
        id = RequestID
    },
    step(is_reply_to_defined, Req, ResponseDTO);
process_subscription(Req, SubRequest = #k1api_subscribe_sms_receipts_request_dto{}) ->
    #k1api_subscribe_sms_receipts_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = _UserID,
        url = NotifyURL,
        dest_addr = SourceAddr,  %% TODO: HERE MUST BE source_addr
        callback_data = CallbackData
    } = SubRequest,
    {ok, QName} = application:get_env(k_handlers, oneapi_receipt_sms_queue),
    Subscription = #k_mb_k1api_receipt_sub{
        id = ID,
        customer_id = CustomerID,
        user_id = <<"undefined">>,
        queue_name = QName,
        source_addr = SourceAddr,
        notify_url = NotifyURL,
        callback_data = CallbackData,
        created_at = ac_datetime:utc_timestamp()
    },
    k_mailbox:register_subscription(Subscription),
    ResponseDTO = #k1api_subscribe_sms_receipts_response_dto{
        id = ID
    },
    step(is_reply_to_defined, Req, ResponseDTO);
process_subscription(Req, UnsubRequest = #k1api_unsubscribe_sms_receipts_request_dto{}) ->
    #k1api_unsubscribe_sms_receipts_request_dto{
        id = ReqID,
        customer_id = CustomerID,
        user_id = _UserID,
        subscription_id = SubscriptionID
    } = UnsubRequest,
    UserID = <<"undefined">>,
    ok = k_mailbox:unregister_subscription(SubscriptionID, CustomerID, UserID),
    ResponseDTO = #k1api_unsubscribe_sms_receipts_response_dto{
        id = ReqID
    },
    step(is_reply_to_defined, Req, ResponseDTO).

step(is_reply_to_defined, Req, DTO) ->
    case k_amqp_req:reply_to(Req) of
        {ok, undefined} ->
            % reply_to is undefined, sekip req
            ?log_warn("reply_to is undefined. skip request", []),
            {ok, []};
        {ok, _ReplyTo} ->
            % reply_to is defined, reply
            step(reply, Req, DTO)
    end;

step(reply, Req, DTO) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    ?log_debug("Send response {~p : ~p}", [DTO, ContentType]),
    case adto:encode(DTO) of
        {ok, Binary} ->
            {ok, ReplyTo} = k_amqp_req:reply_to(Req),
            Reply = #worker_reply{
                reply_to = ReplyTo,
                content_type = ContentType,
                payload = Binary},
            {ok, [Reply]};
        Error ->
            ?log_warn("Unexpected encode error: ~p", [Error]),
            {ok, []}
    end.
