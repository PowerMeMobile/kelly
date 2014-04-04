-module(k_k1api_retrieve_sms_req_handler).

%TODO: remove user_id = undefined

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, Payload} = k_amqp_req:payload(Req),
    case adto:decode(#k1api_retrieve_sms_request_dto{}, Payload) of
        {ok, RetrieveRequest} ->
            process_retrive_sms_request(Req, RetrieveRequest);
        Error ->
            ?log_error("Retrive sms decode error: ~p", [Error]),
            {ok, []}
    end.

%% ===================================================================
%% Interal
%% ===================================================================

process_retrive_sms_request(Req, RetrieveRequest) ->
    ?log_debug("Got retrieve sms request: ~p", [RetrieveRequest]),
    #k1api_retrieve_sms_request_dto{
        id = RequestID,
        customer_id = CustomerID,
        user_id = _UserID,
        dest_addr = DestAddr,
        batch_size = BatchSize
    } = RetrieveRequest,
    UserID = <<"undefined">>,
    {ok, IncomingSms, Total} = k_mailbox:get_incoming_sms(CustomerID, UserID, DestAddr, BatchSize),
    build_response(Req, RequestID, IncomingSms, Total).

build_response(Req, RequestID, IncomingSms, Total) ->
    ?log_debug("RequestID: ~p, IncomingSms: ~p, Total: ~p",
        [RequestID, IncomingSms, Total]),
    MessagesDTO = lists:map(fun(PendingItem) ->
    #k_mb_incoming_sms{
        id = ItemID,
        source_addr = SourceAddr,
        received = Time,
        message_body = Message
    } = PendingItem,
        #k1api_retrieved_sms_dto{
            datetime = Time,
            sender_addr = SourceAddr,
            message_id = ItemID,
            message = Message
        }
    end, IncomingSms),
    DTO = #k1api_retrieve_sms_response_dto{
        id = RequestID,
        messages = MessagesDTO,
        total = Total
    },
    ?log_debug("DTO: ~p", [DTO]),
    step(is_reply_to_defined, Req, DTO).

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
                content_type = <<"OneAPIRetrievedSmsResponse">>,
                payload = Binary
            },
            {ok, [Reply]};
        Error ->
            ?log_warn("Unexpected retrive sms encode error: ~p", [Error]),
            {ok, []}
    end.
