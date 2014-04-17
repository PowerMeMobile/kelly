-module(k_retrieve_sms_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
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
    %UserId = <<"undefined">>,
    case k_mailbox:get_incoming_sms(CustomerId, UserId, DestAddr, BatchSize) of
        {ok, IncomingSms, Total} ->
            build_response(ReqId, IncomingSms, Total);
        Error ->
            Error
    end.

%% ===================================================================
%% Interal
%% ===================================================================

build_response(ReqId, IncomingSms, Total) ->
    ?log_debug("RequestId: ~p, IncomingSms: ~p, Total: ~p",
        [ReqId, IncomingSms, Total]),
    MessagesDTO = lists:map(
        fun(PendingItem) ->
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
    RespDTO = #k1api_retrieve_sms_response_dto{
        id = ReqId,
        messages = MessagesDTO,
        total = Total
    },
    {ok, RespDTO}.
