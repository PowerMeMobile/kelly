-module(k_sms_request_handler).

-export([process/1]).
-compile({no_auto_import, [split_binary/2]}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
   -compile(export_all).
-endif.

-record(port_addressing, {
    dst_port :: non_neg_integer(),
    src_port :: non_neg_integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(ContentType, Payload).

%% ===================================================================
%% Internal
%% ===================================================================

process(<<"SmsRequest">>, ReqBin) ->
    case adto:decode(#just_sms_request_dto{}, ReqBin) of
        {ok, SmsReq} ->
            process_sms_request(SmsReq);
        Error ->
            Error
    end;
process(<<"SmsRequest2">>, ReqBin) ->
    case adto:decode(#just_sms_request_dto{}, ReqBin) of
        {ok, SmsReq} ->
            process_sms_request(SmsReq);
        Error ->
            Error
    end;
process(<<"OneAPISmsRequest">>, ReqBin) ->
    case adto:decode(#just_sms_request_dto{}, ReqBin) of
        {ok, SmsReq} ->
            process_sms_request(SmsReq);
        Error ->
            Error
    end;
process(ReqCT, ReqBin) ->
    ?log_error("Got unknown sms request: ~p ~p", [ReqCT, ReqBin]),
    {ok, []}.

-spec process_sms_request(#just_sms_request_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_request(#just_sms_request_dto{client_type = ClientType} = SmsReq) ->
    ?log_debug("Got sms request: ~p", [SmsReq]),
    ReqTime = ac_datetime:utc_timestamp(),
    ReqInfos = sms_request_to_req_info_list(SmsReq, ReqTime),
    case ClientType of
        oneapi ->
            InMsgIds = [InMsgId || #req_info{in_msg_id = InMsgId} <- ReqInfos],
            process_oneapi_req(SmsReq, InMsgIds);
        _ ->
            nop
    end,
    case ac_utils:safe_foreach(
        fun k_dynamic_storage:set_mt_req_info/1, ReqInfos, ok, {error, '_'}
    ) of
        ok ->
            {ok, []};
        Error ->
            Error
    end.

-spec sms_request_to_req_info_list(#just_sms_request_dto{}, erlang:timestamp())
    -> [#req_info{}].
sms_request_to_req_info_list(SmsReq, ReqTime) ->
    #just_sms_request_dto{
        type = Type,
        dest_addrs = {Type, DstAddrs},
        message_ids = MsgIds,
        network_ids = NetIds,
        prices = Prices
    } = SmsReq,
    Quarts =
        case {NetIds, Prices} of
            {[], []} ->
                Undefs = lists:duplicate(length(DstAddrs), undefined),
                ac_lists:zip4(DstAddrs, MsgIds, Undefs, Undefs);
            {NetIds, Prices} ->
                ac_lists:zip4(DstAddrs, MsgIds, NetIds, Prices)
        end,
    build_req_infos(SmsReq, ReqTime, Quarts).

build_req_infos(SmsReq, ReqTime, Pairs) ->
    build_req_infos(SmsReq, ReqTime, Pairs, []).

build_req_infos(_, _, [], Acc) ->
    lists:reverse(Acc);
build_req_infos(SmsReq, ReqTime, [{DstAddr,MsgId,NetId,Price}|Quarts], Acc) ->
    Type = SmsReq#just_sms_request_dto.type,
    ReqInfo =
        case request_type(Type, MsgId) of
            short ->
                [build_short_req_info(SmsReq, ReqTime, DstAddr, MsgId, NetId, Price)];
            {long, MsgIds} ->
                lists:reverse(build_long_req_infos(SmsReq, ReqTime, DstAddr, MsgIds, NetId, Price));
            part ->
                [build_part_req_info(SmsReq, ReqTime, DstAddr, MsgId, NetId, Price)]
        end,
    build_req_infos(SmsReq, ReqTime, Quarts, ReqInfo ++ Acc).

build_short_req_info(#just_sms_request_dto{
    id = ReqId,
    client_type = ClientType,
    customer_id = CustomerId,
    user_id = UserId,
    gateway_id = GatewayId,
    type = Type,
    message = Body,
    encoding = Encoding,
    params = Params,
    source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId, NetId, Price) ->
    RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
    EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
    ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
    #req_info{
        req_id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = ClientType,
        in_msg_id = InMsgId,
        gateway_id = GatewayId,
        type = Type,
        encoding = Encoding,
        body = Body,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        reg_dlr = RegDlr,
        esm_class = EsmClass,
        val_period = ValPeriod,
        req_time = ReqTime,
        network_id = NetId,
        price = Price
    }.

build_part_req_info(#just_sms_request_dto{
    id = ReqId,
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
    gateway_id = GatewayId,
    type = part,
    message = Body,
    encoding = Encoding,
    params = Params,
    source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId, NetId, Price) ->
    PartRefNum = DstAddr#addr.ref_num,
    PartRefNum = get_param_by_name(<<"sar_msg_ref_num">>, Params, undefined),
    PartSeqNum = get_param_by_name(<<"sar_segment_seqnum">>, Params, undefined),
    PartsTotal = get_param_by_name(<<"sar_total_segments">>, Params, undefined),

    RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
    EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
    ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
    #req_info{
        req_id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = ClientType,
        in_msg_id = InMsgId,
        gateway_id = GatewayId,
        type = {part, #part_info{ref = PartRefNum, seq = PartSeqNum, total = PartsTotal}},
        encoding = Encoding,
        body = Body,
        src_addr = SrcAddr,
        dst_addr = DstAddr#addr{ref_num = undefined},
        reg_dlr = RegDlr,
        esm_class = EsmClass,
        val_period = ValPeriod,
        req_time = ReqTime,
        network_id = NetId,
        price = Price
    }.

build_long_req_infos(SmsReq, ReqTime, DstAddr, InMsgIds, NetId, Price) ->
    Body = SmsReq#just_sms_request_dto.message,
    Encoding = SmsReq#just_sms_request_dto.encoding,
    Params = SmsReq#just_sms_request_dto.params,
    {Encoding2, _DC, Bitness} = encoding_dc_bitness(Encoding, Params, default_gateway_settings()),
    EncodedBody = encode_msg(Body, Encoding2),
    PortAddressing = port_addressing(Params),

    PartsTotal = length(InMsgIds),
    PartSeqNums = lists:seq(1, PartsTotal),
    BodyParts = split_msg(EncodedBody, Bitness, PortAddressing),
    [
        build_long_part_req_info(
            SmsReq, ReqTime, DstAddr, InMsgId, BodyPart, InMsgIds, PartSeqNum, PartsTotal,
            NetId, Price
        ) || {InMsgId, BodyPart, PartSeqNum} <- lists:zip3(InMsgIds, BodyParts, PartSeqNums)
    ].

build_long_part_req_info(#just_sms_request_dto{
    id = ReqId,
    client_type = ClientType,
    customer_id = CustomerId,
    user_id = UserId,
    gateway_id = GatewayId,
    encoding = Encoding,
    params = Params,
    source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId, BodyPart, InMsgIds, PartSeqNum, PartsTotal, NetId, Price) ->
    RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
    EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
    ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
    #req_info{
        req_id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = ClientType,
        in_msg_id = InMsgId,
        gateway_id = GatewayId,
        type = {part, #part_info{
            ref = lists:delete(InMsgId, InMsgIds),
            seq = PartSeqNum,
            total = PartsTotal
        }},
        encoding = Encoding,
        body = BodyPart,
        src_addr = SrcAddr,
        dst_addr = DstAddr,
        reg_dlr = RegDlr,
        esm_class = EsmClass,
        val_period = ValPeriod,
        req_time = ReqTime,
        network_id = NetId,
        price = Price
    }.

-spec get_param_by_name(binary(), [#just_sms_request_param_dto{}], term()) -> term().
get_param_by_name(Name, Params, Default) ->
    case lists:keyfind(Name, #just_sms_request_param_dto.name, Params) of
        false ->
            Default;
        #just_sms_request_param_dto{value = {_, Value}} ->
            Value
    end.

process_oneapi_req(#just_sms_request_dto{
    client_type = oneapi,
    id = ReqId,
    customer_id = CustomerId,
    user_id = UserId,
    params = Params,
    source_addr = SrcAddr
}, InMsgIds) ->
    NotifyURL = get_param_by_name(<<"oneapi_notify_url">>, Params, undefined),
    CallbackData = get_param_by_name(<<"oneapi_callback_data">>, Params, undefined),
    ?log_debug("NotifyURL: ~p CallbackData: ~p", [NotifyURL, CallbackData]),
    case create_oneapi_receipt_subscription(CustomerId, UserId, SrcAddr,
            NotifyURL, CallbackData, ReqId, InMsgIds) of
        {ok, SubId} ->
            ?log_debug("Create receipt subscription: ~p", [SubId]),
            ok;
        nop ->
            ?log_debug("Receipt subscription didn't requested", []),
            ok
    end;
process_oneapi_req(_, _) ->
    ok.

create_oneapi_receipt_subscription(_, _, _, undefined, _, _, _) -> nop;
create_oneapi_receipt_subscription(CustomerId, UserId, SrcAddr,
        NotifyURL, CallbackData, ReqId, InMsgIds) ->
    {ok, QName} = application:get_env(k_handlers, oneapi_incoming_sms_queue),
    SubId = uuid:unparse(uuid:generate_time()),
    Sub = #k_mb_k1api_receipt_sub{
        id = SubId,
        customer_id = CustomerId,
        user_id = UserId,
        queue_name = QName,
        src_addr = SrcAddr,
        notify_url = NotifyURL,
        callback_data = CallbackData,
        req_id = ReqId,
        in_msg_ids = InMsgIds,
        created_at = ac_datetime:utc_timestamp()
    },
    ok = k_mailbox:register_sms_req_receipts_subscription(Sub),
    {ok, SubId}.

%% ===================================================================
%% Message splitting logic from Just
%% ===================================================================

default_gateway_settings() -> [
    {default_encoding, gsm0338},
    {default_data_coding, 0},
    {default_bitness, 7}
].

request_type(Type, MsgId) ->
    case {Type, binary:split(MsgId, <<":">>, [global, trim])} of
        {regular, [MsgId]} ->
            short;
        {regular, MsgIds} ->
            {long, MsgIds};
        {part, [MsgId]} ->
            part
    end.

encoding_dc_bitness(Encoding, Params, Settings) ->
    {E, DC, B} =
        case Encoding of
            default -> {
                ?gv(default_encoding, Settings),
                ?gv(default_data_coding, Settings),
                ?gv(default_bitness, Settings)
            };
            gsm0338 ->
                {gsm0338, 0, 7};
            ascii ->
                {ascii, 1, 7};
            latin1 ->
                {latin1, 3, 8};
            ucs2 ->
                {ucs2, 8, 16};
            Other ->
                {other, Other, 8}
        end,
    {E, ?gv(data_coding, Params, DC), B}.

encode_msg(Msg, gsm0338) ->
    {valid, Encoded} = gsm0338:from_utf8(Msg),
    Encoded;
encode_msg(Msg, ascii) ->
    Msg;
encode_msg(Msg, latin1) ->
    {ok, Encoded} = iconverl:conv("latin1//IGNORE", "utf-8", Msg),
    Encoded;
encode_msg(Msg, ucs2) ->
    {ok, Encoded} = iconverl:conv("ucs-2be//IGNORE", "utf-8", Msg),
    Encoded;
encode_msg(Msg, other) ->
    Msg.

port_addressing(Params) ->
    DstPort = get_param_by_name(<<"destination_port">>, Params, undefined),
    SrcPort = get_param_by_name(<<"source_port">>, Params, undefined),
    case DstPort =/= undefined andalso SrcPort =/= undefined of
        true ->
            #port_addressing{dst_port = DstPort, src_port = SrcPort};
        false ->
            undefined
    end.

max_msg_len(Bitness, undefined) ->
    case Bitness of
        7  -> {160, 153};
        8  -> {140, 134};
        16 -> {140, 134}
    end;
max_msg_len(Bitness, _) ->
    case Bitness of
        7  -> {152, 146};
        8  -> {133, 128};
        16 -> {132, 128}
    end.

split_msg(Msg, Bitness, PortAddressing) ->
    {MaxWhole, MaxPart} = max_msg_len(Bitness, PortAddressing),
    case size(Msg) > MaxWhole of
        true  ->
            split_binary(Msg, MaxPart);
        false ->
            [Msg]
    end.

-spec split_binary(binary(), pos_integer()) -> [binary()].
split_binary(Bin, Len) when Len > 0 ->
    split_binary(Bin, Len, []).

split_binary(Bin, Len, Acc) ->
    case size(Bin) of
        0 ->
            lists:reverse(Acc);
        N when N =< Len ->
            lists:reverse([Bin | Acc]);
        _ ->
            {Bin1, Bin2} = erlang:split_binary(Bin, Len),
            split_binary(Bin2, Len, [Bin1 | Acc])
    end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

sms_request_to_req_info_list_regular_short_batch_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"Hello">>,
    Encoding = ascii,
    ReqTime = {0,0,0},

    SmsReq = #just_sms_request_dto{
        id = RID,
        gateway_id = GID,
        customer_id = CID,
        user_id = UID,
        client_type = CT,
        type = regular,
        message = Body,
        encoding = Encoding,
        params = [
            #just_sms_request_param_dto{
                name = <<"registered_delivery">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"service_type">>,
                value = {string,<<>>}
            },
            #just_sms_request_param_dto{
                name = <<"no_retry">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"validity_period">>,
                value = {string,<<"000003000000000R">>}
            },
            #just_sms_request_param_dto{
                name = <<"priority_flag">>,
                value = {integer,0}
            },
            #just_sms_request_param_dto{
                name = <<"esm_class">>,
                value = {integer,3}
            },
            #just_sms_request_param_dto{
                name = <<"protocol_id">>,
                value = {integer,0}
            }
        ],
        source_addr = #addr{addr = <<"0">>},
        dest_addrs = {regular,[
            #addr{addr = <<"1">>},
            #addr{addr = <<"2">>}
        ]},
        message_ids = [<<"1">>, <<"2">>],
        network_ids = [<<"NID1">>, <<"NID2">>],
        prices = [1.0, 2.0]
    },
    Expected = [
        #req_info{
           req_id = RID,
           customer_id = CID,
           user_id = UID,
           client_type = CT,
           in_msg_id = <<"1">>,
           gateway_id = GID,
           type = regular,
           encoding = Encoding,
           body = Body,
           src_addr = #addr{addr = <<"0">>},
           dst_addr = #addr{addr = <<"1">>},
           reg_dlr = false,
           esm_class = 3,
           val_period = <<"000003000000000R">>,
           req_time = ReqTime,
           network_id = <<"NID1">>,
           price = 1.0
        },
        #req_info{
           req_id = RID,
           customer_id = CID,
           user_id = UID,
           client_type = CT,
           in_msg_id = <<"2">>,
           gateway_id = GID,
           type = regular,
           encoding = Encoding,
           body = Body,
           src_addr = #addr{addr = <<"0">>},
           dst_addr = #addr{addr = <<"2">>},
           reg_dlr = false,
           esm_class = 3,
           val_period = <<"000003000000000R">>,
           req_time = ReqTime,
           network_id = <<"NID2">>,
           price = 2.0
        }
    ],
    ?assertEqual(Expected, sms_request_to_req_info_list(SmsReq, ReqTime)).

sms_request_to_req_info_list_part_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
    Encoding = ascii,
    ReqTime = {0,0,0},

    SmsReq = #just_sms_request_dto{
        id = RID,
        gateway_id = GID,
        customer_id = CID,
        user_id = UID,
        client_type = CT,
        type = part,
        message = Body,
        encoding = Encoding,
        params = [
            #just_sms_request_param_dto{
                name = <<"data_coding">>,
                value = {integer,240}
            },
            #just_sms_request_param_dto{
                name = <<"sar_msg_ref_num">>,
                value = {integer,249}
            },
            #just_sms_request_param_dto{
                name = <<"sar_total_segments">>,
                value = {integer,3}
            },
            #just_sms_request_param_dto{
                name = <<"sar_segment_seqnum">>,
                value = {integer,1}
            },
            #just_sms_request_param_dto{
                name = <<"registered_delivery">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"service_type">>,
                value = {string,<<>>}
            },
            #just_sms_request_param_dto{
                name = <<"no_retry">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"validity_period">>,
                value = {string,<<"000003000000000R">>}
            },
            #just_sms_request_param_dto{
                name = <<"priority_flag">>,
                value = {integer,0}
            },
            #just_sms_request_param_dto{
                name = <<"esm_class">>,
                value = {integer,0}
            },
            #just_sms_request_param_dto{
                name = <<"protocol_id">>,
                value = {integer,0}
            }
        ],
        source_addr = #addr{addr = <<"0">>},
        dest_addrs = {part,[
            #addr{addr = <<"1">>, ref_num = 249}
        ]},
        message_ids = [<<"1">>],
        network_ids = [<<"NID1">>],
        prices = [1.0]
    },
    Expected = [
        #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"1">>,
            gateway_id = GID,
            type = {part, #part_info{ref = 249, seq = 1, total = 3}},
            encoding = Encoding,
            body = Body,
            src_addr = #addr{addr = <<"0">>},
            dst_addr = #addr{addr = <<"1">>},
            reg_dlr = false,
            esm_class = 0,
            val_period = <<"000003000000000R">>,
            req_time = ReqTime,
            network_id = <<"NID1">>,
            price = 1.0
        }
    ],
    Actual = sms_request_to_req_info_list(SmsReq, ReqTime),
    ?assertEqual(Expected, Actual).

sms_request_to_req_info_list_multipart_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222223333333">>,
    Encoding = ascii,
    ReqTime = {0,0,0},

    SmsReq = #just_sms_request_dto{
        id = RID,
        gateway_id = GID,
        customer_id = CID,
        user_id = UID,
        client_type = CT,
        type = regular,
        message = Body,
        encoding = Encoding,
        params = [
            #just_sms_request_param_dto{
                name = <<"registered_delivery">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"service_type">>,
                value = {string,<<>>}
            },
            #just_sms_request_param_dto{
                name = <<"no_retry">>,
                value = {boolean,false}
            },
            #just_sms_request_param_dto{
                name = <<"validity_period">>,
                value = {string,<<"000003000000000R">>}
            },
            #just_sms_request_param_dto{
                name = <<"priority_flag">>,
                value = {integer,0}
            },
            #just_sms_request_param_dto{
                name = <<"esm_class">>,
                value = {integer,0}
            },
            #just_sms_request_param_dto{
                name = <<"protocol_id">>,
                value = {integer,0}
            }
        ],
        source_addr = #addr{addr = <<"0">>},
        dest_addrs = {regular,[
            #addr{addr = <<"1">>}
        ]},
        message_ids = [<<"1:2:3">>],
        network_ids = [<<"NID1">>],
        prices = [1.0]
    },
    Expected = [
        #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"1">>,
            gateway_id = GID,
            type = {part, #part_info{ref = [<<"2">>, <<"3">>], seq = 1, total = 3}},
            encoding = Encoding,
            body = <<"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
            src_addr = #addr{addr = <<"0">>},
            dst_addr = #addr{addr = <<"1">>},
            reg_dlr = false,
            esm_class = 0,
            val_period = <<"000003000000000R">>,
            req_time = ReqTime,
            network_id = <<"NID1">>,
            price = 1.0
        },
        #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"2">>,
            gateway_id = GID,
            type = {part, #part_info{ref = [<<"1">>, <<"3">>], seq = 2, total = 3}},
            encoding = Encoding,
            body = <<"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222">>,
            src_addr = #addr{addr = <<"0">>},
            dst_addr = #addr{addr = <<"1">>},
            reg_dlr = false,
            esm_class = 0,
            val_period = <<"000003000000000R">>,
            req_time = ReqTime,
            network_id = <<"NID1">>,
            price = 1.0
        },
        #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"3">>,
            gateway_id = GID,
            type = {part, #part_info{ref = [<<"1">>, <<"2">>], seq = 3, total = 3}},
            encoding = Encoding,
            body = <<"3333333">>,
            src_addr = #addr{addr = <<"0">>},
            dst_addr = #addr{addr = <<"1">>},
            reg_dlr = false,
            esm_class = 0,
            val_period = <<"000003000000000R">>,
            req_time = ReqTime,
            network_id = <<"NID1">>,
            price = 1.0
        }
    ],
    Actual = sms_request_to_req_info_list(SmsReq, ReqTime),
    ?assertEqual(Expected, Actual).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
