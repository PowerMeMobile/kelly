-module(k_k1api_delivery_status_req_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, Payload} = k_amqp_req:payload(Req),
    case adto:decode(#k1api_sms_delivery_status_request_dto{}, Payload) of
        {ok, StatusRequest} ->
            process_delivery_status_request(Req, StatusRequest);
        Error ->
            ?log_error("Deliver status decode error: ~p", [Error]),
            {ok, []}
    end.

%% ===================================================================
%% Interal
%% ===================================================================

process_delivery_status_request(Req, StatusRequest) ->
    ?log_debug("Got sms delivery status request: ~p", [StatusRequest]),
    #k1api_sms_delivery_status_request_dto{
        id = RequestID,
        sms_request_id = SmsRequestID
    } = StatusRequest,
    {ok, Msgs} = shifted_storage:find(mt_messages, {ri, SmsRequestID}),
    DTO = #k1api_sms_delivery_status_response_dto{
        id = RequestID,
        statuses = [status(Msg) || Msg <- Msgs]
    },
    step(is_reply_to_defined, Req, DTO).

status({_ID, MsgDoc}) ->
    #k1api_sms_status_dto{
        address = k_storage_utils:doc_to_addr(bson:at(da, MsgDoc)),
        status = bson:at(s, MsgDoc)
    }.

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
    case adto:encode(DTO) of
        {ok, Binary} ->
            {ok, ReplyTo} = k_amqp_req:reply_to(Req),
            Reply = #worker_reply{
                reply_to = ReplyTo,
                content_type = <<"OneAPIDeliveryStatusResponse">>,
                payload = Binary},
            {ok, [Reply]};
        Error ->
            ?log_warn("Unexpected delivery status encode error: ~p", [Error]),
            {ok, []}
    end.
