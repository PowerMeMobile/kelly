-module(k_retrieve_sms_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_retrieve_sms_request_dto{}) ->
    {ok, #k1api_retrieve_sms_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId      = ReqDTO#k1api_retrieve_sms_request_dto.id,
    CustomerId = ReqDTO#k1api_retrieve_sms_request_dto.customer_id,
    UserId     = ReqDTO#k1api_retrieve_sms_request_dto.user_id,
    DestAddr   = ReqDTO#k1api_retrieve_sms_request_dto.dest_addr,
    BatchSize  = ReqDTO#k1api_retrieve_sms_request_dto.batch_size,
    case k_mailbox:get_incoming_sms(CustomerId, UserId, DestAddr, BatchSize) of
        {ok, Messages, Pending} ->
            {ok, Response} = build_response(ReqId, Messages, Pending),
            delete_retrieved(Messages),
            {ok, Response};
        Error ->
            Error
    end.

%% ===================================================================
%% Interal
%% ===================================================================

build_response(ReqId, Messages, Pending) ->
    MessagesDTO = lists:map(
        fun(Message) ->
            #k_mb_incoming_sms{
                id = ItemID,
                src_addr = SrcAddr,
                received = Time,
                body = Body
            } = Message,
            #k1api_retrieved_sms_dto{
                datetime = Time,
                sender_addr = SrcAddr,
                message_id = ItemID,
                message = Body
            }
        end, Messages),
    RespDTO = #k1api_retrieve_sms_response_dto{
        id = ReqId,
        messages = MessagesDTO,
        total = Pending
    },
    {ok, RespDTO}.

delete_retrieved(Messages) ->
    [k_mailbox:delete_item(M) || M <- Messages].
