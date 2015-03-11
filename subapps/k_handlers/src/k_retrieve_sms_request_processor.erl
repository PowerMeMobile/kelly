-module(k_retrieve_sms_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #k1api_retrieve_sms_request_dto{}) ->
    ReqId      = Req#k1api_retrieve_sms_request_dto.id,
    CustomerId = Req#k1api_retrieve_sms_request_dto.customer_id,
    UserId     = Req#k1api_retrieve_sms_request_dto.user_id,
    DestAddr   = Req#k1api_retrieve_sms_request_dto.dest_addr,
    BatchSize  = Req#k1api_retrieve_sms_request_dto.batch_size,
    %% TODO: Ensure correct DestAddr for Customer:User
    case k_mailbox:get_incoming_sms(CustomerId, UserId, DestAddr, BatchSize) of
        {ok, Messages, Pending} ->
            MessagesDTO = [incoming_sms_to_dto(M) || M <- Messages],
            Resp = #k1api_retrieve_sms_response_dto{
                id = ReqId,
                messages = MessagesDTO,
                total = Pending
            },
            delete_retrieved(Messages),
            {ok, Resp};
        Error ->
            Error
    end;
process(Req = #retrieve_sms_req_v1{}) ->
    ReqId      = Req#retrieve_sms_req_v1.req_id,
    CustomerId = Req#retrieve_sms_req_v1.customer_id,
    UserId     = Req#retrieve_sms_req_v1.user_id,
    DstAddr    = Req#retrieve_sms_req_v1.dst_addr,
    BatchSize  = Req#retrieve_sms_req_v1.batch_size,
    %% TODO: Ensure correct DestAddr for Customer:User
    case k_mailbox:get_incoming_sms(CustomerId, UserId, DstAddr, BatchSize) of
        {ok, Messages, Pending} ->
            MessagesDTO = [incoming_sms_to_v1(M) || M <- Messages],
            Resp = #retrieve_sms_resp_v1{
                req_id = ReqId,
                messages = MessagesDTO,
                pending = Pending
            },
            delete_retrieved(Messages),
            {ok, Resp};
        Error ->
            Error
    end.

%% ===================================================================
%% Interal
%% ===================================================================

incoming_sms_to_dto(Msg) ->
    #k_mb_incoming_sms{
        id = ItemId,
        src_addr = SrcAddr,
        received = RecvTime,
        body = Body
    } = Msg,
    #k1api_retrieved_sms_dto{
        datetime = RecvTime,
        sender_addr = SrcAddr,
        message_id = ItemId,
        message = Body
     }.

incoming_sms_to_v1(Msg) ->
    #k_mb_incoming_sms{
        id = ItemId,
        src_addr = SrcAddr,
        received = RecvTime,
        body = Body
    } = Msg,
    #msg_info_v1{
        msg_id = ItemId,
        src_addr = SrcAddr,
        body = Body,
        recv_time = RecvTime
     }.

delete_retrieved(Messages) ->
    [k_mailbox:delete_item(M) || M <- Messages].
