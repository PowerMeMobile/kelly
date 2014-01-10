-module(k_k1api_delivery_status_req_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#k1api_sms_delivery_status_request_dto{}, Message) of
		{ok, Request} ->
			process_delivery_status_request(Request);
		Error ->
			?log_error("k1api dto decode error: ~p", [Error]),
			{ok, []}
	end.

%% ===================================================================
%% Interal
%% ===================================================================

process_delivery_status_request(Request) ->
	?log_debug("Got k1api sms delivery status request: ~p", [Request]),
	#k1api_sms_delivery_status_request_dto{
		id = RequestID,
		sms_request_id = SmsRequestID
	} = Request,
	{ok, Msgs} = k_shifted_storage:find(mt_messages, {ri, SmsRequestID}),
	DTO = #k1api_sms_delivery_status_response_dto{
		id = RequestID,
		statuses = [status(Msg) || Msg <- Msgs]
	},
	reply(DTO).

status({_ID, MsgDoc}) ->
	#k1api_sms_status_dto{
		address = k_storage_utils:doc_to_addr(bson:at(da, MsgDoc)),
		status = bson:at(s, MsgDoc)
	}.

reply(DTO) ->
	case adto:encode(DTO) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.k1api.delivery_status_response">>,
				content_type = <<"OneAPIDeliveryStatusResponse">>,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api dto encode error: ~p", [Error]),
	   		{ok, []}
	end.
