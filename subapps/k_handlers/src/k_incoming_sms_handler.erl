-module(k_incoming_sms_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include_lib("k_mailbox/include/application.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(ContentType, Payload).

%% ===================================================================
%% Internals
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"IncomingSm">>, Message) ->
    case adto:decode(#just_incoming_sms_dto{}, Message) of
        {ok, IncomingSmsRequest} ->
            process_incoming_sms_request(IncomingSmsRequest);
        Error ->
            Error
    end;

process(CT, Message) ->
    ?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
    {ok, []}.

process_incoming_sms_request(IncSmsRequest = #just_incoming_sms_dto{
    gateway_id = GatewayId,
    source = SourceAddr,
    dest = DestAddr,
    message = MessageBody,
    data_coding = DataCoding,
    parts_ref_num = _PartsRefNum,
    parts_count = _PartsCount,
    part_index = _PartIndex,
    timestamp = UTCString
}) ->
    ?log_debug("Got incoming sms request: ~p ", [IncSmsRequest]),

    ItemId = uuid:unparse(uuid:generate_time()),
    Timestamp = ac_datetime:utc_string_to_timestamp(UTCString),

    %% try to determine customer id and user id,
    %% this will return
    %% {customer_id, user_id} | {customer_id, undefined} | {undefined, undefined}.
    %% i think it makes sense to store even partly filled message.
    {CustomerId, UserId} =
        case k_addr2cust:resolve(DestAddr) of
            {ok, CID, UID} ->
                ?log_debug("Got incoming message from dest_addr: ~p for customer uuid: ~p, user id: ~p",
                    [DestAddr, CID, UID]),
                Item = #k_mb_incoming_sms{
                    id = ItemId,
                    customer_id = CID,
                    user_id = UID,
                    source_addr = SourceAddr,
                    dest_addr = DestAddr,
                    received  = Timestamp,
                    message_body = MessageBody,
                    encoding = DataCoding
                },
                k_mailbox:register_incoming_item(Item),
                ?log_debug("Incoming message registered with id: ~p", [ItemId]),
                {CID, UID};
            Error ->
                ?log_debug("Address resolution failed with: ~p", [Error]),
                ?log_debug("Couldn't resolve incoming message coming to: ~p", [DestAddr]),
                {undefined, undefined}
        end,
    MsgInfo = #msg_info{
        msg_id = ItemId,
        gateway_id = GatewayId,
        customer_id = CustomerId,
        user_id = UserId,
        type = regular,
        encoding = DataCoding,
        body = MessageBody,
        src_addr = SourceAddr,
        dst_addr = DestAddr,
        reg_dlr = false,
        req_time = Timestamp
    },
    ok = k_dynamic_storage:set_mo_msg_info(MsgInfo),
    {ok, []}.
