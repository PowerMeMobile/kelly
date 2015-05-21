-module(k_retrieve_incoming_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #retrieve_incoming_req_v1{}) ->
    ReqId      = Req#retrieve_incoming_req_v1.req_id,
    CustomerUuid = Req#retrieve_incoming_req_v1.customer_uuid,
    UserId     = Req#retrieve_incoming_req_v1.user_id,
    DstAddr    = Req#retrieve_incoming_req_v1.dst_addr,
    BatchSize  = Req#retrieve_incoming_req_v1.batch_size,
    %% TODO: Ensure correct DestAddr for Customer:User
    case k_mailbox:get_incoming(CustomerUuid, UserId, DstAddr, BatchSize) of
        {ok, Messages, Pending} ->
            MessagesDTO = [incoming_to_v1(M) || M <- Messages],
            Resp = #retrieve_incoming_resp_v1{
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

incoming_to_v1(Msg) ->
    #k_mb_incoming{
        id = ItemId,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        body = Body,
        rcv_time = RcvTime,
        state = State
    } = Msg,
    #inbox_msg_info_v1{
        msg_id = ItemId,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        body = Body,
        size = size(Body),
        rcv_time = RcvTime,
        state = State
     }.

delete_retrieved(Messages) ->
    [k_mailbox:delete_item(M) || M <- Messages].
