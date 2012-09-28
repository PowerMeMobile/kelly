-module(k_k1api_subscribe_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_mailbox/include/subscription.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(CT = <<"SubscribeIncomingSms">>, Message) ->
	case adto:decode(#k1api_subscribe_incoming_sms_request_dto{}, Message) of
		{ok, Request} ->
			?log_debug("Got subscribe incoming sms request: ~p", [Request]),
			process_subscription(Request, CT);
		Error ->
			?log_error("k1api dto decode error: ~p", [Error]),
			{ok, []}
	end;
process(CT = <<"UnsubscribeIncomingSms">>, Message) ->
	case adto:decode(#k1api_unsubscribe_incoming_sms_request_dto{}, Message) of
		{ok, Request} ->
			?log_debug("Got unsubscribe incoming sms request: ~p", [Request]),
			process_subscription(Request, CT);
		Error ->
			?log_error("k1api dto decode error: ~p", [Error]),
			{ok, []}
	end;
process(ContentType, _Bin) ->
	?log_warn("Got unexpected message type: ~p", [ContentType]),
	{ok, []}.

%% ===================================================================
%% Interal
%% ===================================================================

process_subscription(Request = #k1api_subscribe_incoming_sms_request_dto{}, CT = <<"SubscribeIncomingSms">>) ->
	#k1api_subscribe_incoming_sms_request_dto{
		id = ID,
		customer_id = CustomerID,
		user_id = _UserID,
		dest_addr = _AddrDTO,
		notify_url = URL,
		criteria = Criteria,
		correlator = Correlator,
		callback_data = Callback
	} = Request,
	QName = <<"pmm.k1api.incoming_sms">>,
	Subscription = #k_mb_subscription{
		id = ID,
		customer_id = CustomerID,
		user_id = <<"undefined">>,
	   	type = incomingSMSReceiver,
		priority = 1,
		app_type = oneapi,
		queue_name = QName,
		notify_url = URL,
		criteria = Criteria,
		callback_data = Callback,
		correlator = Correlator
	},
	k_mailbox:register_subscription(Subscription),
	ResponseDTO = #k1api_subscribe_incoming_sms_response_dto{
		id = ID,
		subscription_id = ID
	},
	reply(ResponseDTO, CT);
process_subscription(Request = #k1api_unsubscribe_incoming_sms_request_dto{}, CT = <<"UnsubscribeIncomingSms">>) ->
	#k1api_unsubscribe_incoming_sms_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = _UserID,
		subscription_id = SubscriptionID
	} = Request,
	UserID = <<"undefined">>,
	ok = k_mailbox:unregister_subscription(SubscriptionID, CustomerID, UserID),
	ResponseDTO = #k1api_unsubscribe_incoming_sms_response_dto{
		id = RequestID
	},
	reply(ResponseDTO, CT).

reply(DTO, ContentType) ->
	?log_debug("Send response {~p : ~p}", [DTO, ContentType]),
	case adto:encode(DTO) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.k1api.subscription_response">>,
				content_type = ContentType,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api dto encode error: ~p", [Error]),
	   		{ok, []}
	end.
