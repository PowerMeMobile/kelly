-module(k_dynamic_storage).

-export([
    set_mt_req_info/1,
    set_mt_resp_info/1,
    set_mt_dlr_info_and_get_msg_info/1,
    set_mt_downlink_dlr_status/4,

    set_mt_batch_info/1,

    set_mo_msg_info/1,
    set_mo_downlink_dlr_status/3
]).

-include("msg_info.hrl").
-include("customer.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

-type reason() :: any().

-define(UNKNOWN_ADDR,     <<"xxxxxxxxxx">>).
-define(UNKNOWN_BOOL,     false).
-define(UNKNOWN_BODY,
    <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>).
-define(UNKNOWN_ENCODING, <<"xxxxxxxxxx">>).
-define(UNKNOWN_ID,       <<"xxxxxxxxxx">>).
-define(UNKNOWN_INT,      0).
-define(UNKNOWN_FLOAT,    0.0).
-define(UNKNOWN_TIME,     {0,0,0}).
-define(UNKNOWN_TIMEFMT,  <<"xxxxxxxxxxxxxxxx">>).
-define(UNKNOWN_TYPE,     <<"xxxxxxxxxx">>).
-define(UNKNOWN_UUID,     <<"xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx">>).

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(#req_info{}) -> ok | {error, reason()}.
set_mt_req_info(#req_info{req_id = ReqId, in_msg_id = InMsgId} = ReqInfo) ->
    Selector = {
        'ri' , ReqId,
        'imi', InMsgId
    },
    Modifier = set_mt_req_info_modifier(ReqInfo),

    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mt_req_info(Selector, Modifier).

-spec set_mt_resp_info(#resp_info{}) -> ok | {error, reason()}.
set_mt_resp_info(#resp_info{
    req_id = ReqId,
    customer_id = CustomerId,
    in_msg_id = InMsgId,
    client_type = ClientType,
    gateway_id = GatewayId,
    out_msg_id = OutMsgId,
    resp_time = RespTime,
    resp_status = RespStatus,
    resp_error_code = RespErrorCode
}) ->
    Selector = {
        'ri' , ReqId,
        'imi', InMsgId
    },
    Modifier = {
        '$setOnInsert', {
            'ci' , CustomerId,
            'ui' , ?UNKNOWN_ID,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'gi' , GatewayId,
            't'  , ?UNKNOWN_TYPE,
            'e'  , ?UNKNOWN_ENCODING,
            'b'  , ?UNKNOWN_BODY,
            'sa' , ?UNKNOWN_ADDR,
            'da' , ?UNKNOWN_ADDR,
            'rd' , ?UNKNOWN_BOOL,
            'ec' , ?UNKNOWN_INT,
            'vp' , ?UNKNOWN_TIMEFMT,
            'rqt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME,
            'ni' , ?UNKNOWN_ID,
            'p'  , ?UNKNOWN_FLOAT
        },
        '$set',
            case RespErrorCode of
                undefined -> {
                    'omi', OutMsgId,
                    's'  , bsondoc:atom_to_binary(RespStatus),
                    'rpt', RespTime
                };
                _ -> {
                    'omi', OutMsgId,
                    's'  , bsondoc:atom_to_binary(RespStatus),
                    'rpt', RespTime,
                    'rpe', RespErrorCode
                }
            end
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mt_resp_info(Selector, Modifier).

-spec set_mt_dlr_info_and_get_msg_info(#dlr_info{}) -> {ok, #msg_info{}} | {error, reason()}.
set_mt_dlr_info_and_get_msg_info(#dlr_info{
    gateway_id = GatewayId,
    out_msg_id = OutMsgId,
    dlr_time = DlrTime,
    dlr_status = DlrStatus
}) ->
    Selector = {
        'gi' , GatewayId,
        'omi', OutMsgId,
        'rd' , true,
        %% the `sent' status is deprecated
        %% upon removing the `sent' status
        %% simply replace with
        %% s,<<"submitted">>
        '$or', [{s,<<"sent">>}, {s,<<"submitted">>}]
    },
    Sort = {
        'rqt', -1
    },
    Modifier = {
        '$set', {
            's' , bsondoc:atom_to_binary(DlrStatus),
            'dt', DlrTime
        }
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    case StorageMode:set_mt_dlr_info_and_get_msg_info(Selector, Sort, Modifier) of
        {ok, Doc} ->
            {ok, k_storage_utils:doc_to_mt_msg_info(Doc)};
        Error ->
            Error
    end.

-spec set_mt_downlink_dlr_status(req_id(), in_msg_id(), atom(), timestamp()) -> ok.
set_mt_downlink_dlr_status(ReqId, InMsgId, Status, Timestamp) ->
    Selector = {
        'ri' , ReqId,
        'imi', InMsgId
    },
    Modifier = {
        '$set', {
            'dds', bsondoc:atom_to_binary(Status),
            'ddt', Timestamp
        }
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mt_downlink_dlr_status(Selector, Modifier).

-spec set_mt_batch_info(#batch_info{}) -> ok | {error, reason()}.
set_mt_batch_info(#batch_info{
    req_id = ReqId,
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
    src_addr = SrcAddr,
    body = Body,
    req_time = ReqTime,
    recipients = Recipients,
    messages = Messages,
    price = Price
}) ->
    Selector = {
        '_id', ReqId
    },
    Modifier = {
        '$setOnInsert', {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'b'  , Body,
            'rqt', ReqTime
        },
        '$inc', {
            'rs' , Recipients,
            'ms' , Messages,
            'p'  , Price
        }
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mt_batch_info(Selector, Modifier).

-spec set_mo_msg_info(#msg_info{}) -> ok | {error, reason()}.
set_mo_msg_info(MsgInfo = #msg_info{}) ->
    MsgId = MsgInfo#msg_info.msg_id,
    GatewayId = MsgInfo#msg_info.gateway_id,
    CustomerId = MsgInfo#msg_info.customer_id,
    UserId = MsgInfo#msg_info.user_id,
    Type = MsgInfo#msg_info.type,
    Encoding = MsgInfo#msg_info.encoding,
    MessageBody = MsgInfo#msg_info.body,
    SrcAddr = MsgInfo#msg_info.src_addr,
    DstAddr = MsgInfo#msg_info.dst_addr,
    RegDlr = MsgInfo#msg_info.reg_dlr,
    ReqTime = MsgInfo#msg_info.req_time,
    Status = MsgInfo#msg_info.status,

    Selector = {
        '_id', MsgId
    },
    Modifier = {
        '$set' , {
            'gi' , GatewayId,
            'ci' , CustomerId,
            'ui' , UserId,
            't'  , bsondoc:atom_to_binary(Type),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , MessageBody,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'rqt', ReqTime,
            's'  , bsondoc:atom_to_binary(Status)
        }
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mo_msg_info(Selector, Modifier).

-spec set_mo_downlink_dlr_status(msg_id(), atom(), timestamp()) -> ok.
set_mo_downlink_dlr_status(MsgId, Status, Timestamp) ->
    Selector = {
        '_id', MsgId
    },
    Modifier = {
        '$set', {
            'dds', bsondoc:atom_to_binary(Status),
            'ddt', Timestamp
        }
    },
    {ok, StorageMode} = k_storage_manager:get_storage_mode(),
    StorageMode:set_mo_downlink_dlr_status(Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

set_mt_req_info_modifier(#req_info{
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
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
}) when Type =:= regular ->
    {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set',
    if NetId =/= undefined ->
        {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'gi' , GatewayId,
            't'  , bsondoc:atom_to_binary(Type),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime,
            'ni' , NetId,
            'p'  , Price
        };
        true ->
        {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'gi' , GatewayId,
            't'  , bsondoc:atom_to_binary(Type),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        }
    end
    };
set_mt_req_info_modifier(#req_info{
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
    gateway_id = GatewayId,
    type = {Type, #part_info{ref = PartRef, seq = PartSeq, total = PartsTotal}},
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
}) when Type =:= part ->
    {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set',
    if NetId =/= undefined ->
        {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'gi' , GatewayId,
            't'  , {
                'n' , bsondoc:atom_to_binary(Type),
                'r' , PartRef,
                's' , PartSeq,
                't' , PartsTotal
            },
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime,
            'ni' , NetId,
            'p'  , Price
        };
        true ->
        {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'gi' , GatewayId,
            't'  , {
                'n' , bsondoc:atom_to_binary(Type),
                'r' , PartRef,
                's' , PartSeq,
                't' , PartsTotal
            },
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        }
    end
    }.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

set_mt_req_info_modifier_regular_undef_network_id_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"Hello">>,
    Type = regular,
    Encoding = default,
    SrcAddr = #addr{},
    DstAddr = #addr{},
    RegDlr = false,
    EsmClass = 0,
    ValPeriod = <<"000003000000000R">>,
    ReqTime = {0,0,0},
    ReqInfo = #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"3">>,
            gateway_id = GID,
            type = Type,
            encoding = Encoding,
            body = Body,
            src_addr = SrcAddr,
            dst_addr = DstAddr,
            reg_dlr = RegDlr,
            esm_class = EsmClass,
            val_period = ValPeriod,
            req_time = ReqTime,
            network_id = undefined,
            price = undefined
    },
    Modifier = set_mt_req_info_modifier(ReqInfo),
    Expected = {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set', {
            'ci' , CID,
            'ui' , UID,
            'ct' , bsondoc:atom_to_binary(CT),
            'gi' , GID,
            't'  , bsondoc:atom_to_binary(Type),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        }
    },
    %?debugFmt("~p~n", [Expected]),
    %?debugFmt("~p~n", [Modifier]),
    ?assertEqual(Expected, Modifier).

set_mt_req_info_modifier_regular_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"Hello">>,
    Type = regular,
    Encoding = default,
    SrcAddr = #addr{},
    DstAddr = #addr{},
    RegDlr = false,
    EsmClass = 0,
    ValPeriod = <<"000003000000000R">>,
    ReqTime = {0,0,0},
    NID = <<"390a8f74-a60e-11e4-a113-28d2445f2979">>,
    Price = 1.0,
    ReqInfo = #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"3">>,
            gateway_id = GID,
            type = Type,
            encoding = Encoding,
            body = Body,
            src_addr = SrcAddr,
            dst_addr = DstAddr,
            reg_dlr = RegDlr,
            esm_class = EsmClass,
            val_period = ValPeriod,
            req_time = ReqTime,
            network_id = NID,
            price = Price
    },
    Modifier = set_mt_req_info_modifier(ReqInfo),
    Expected = {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set', {
            'ci' , CID,
            'ui' , UID,
            'ct' , bsondoc:atom_to_binary(CT),
            'gi' , GID,
            't'  , bsondoc:atom_to_binary(Type),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime,
            'ni' , NID,
            'p'  , Price
        }
    },
    ?assertEqual(Expected, Modifier).

set_mt_req_info_modifier_part_undef_network_id_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"Hello">>,
    Type = part,
    Encoding = default,
    PartRef = 249,
    PartSeq = 3,
    PartsTotal = 3,
    SrcAddr = #addr{},
    DstAddr = #addr{},
    RegDlr = false,
    EsmClass = 0,
    ValPeriod = <<"000003000000000R">>,
    ReqTime = {0,0,0},
    ReqInfo = #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"3">>,
            gateway_id = GID,
            type = {part, #part_info{ref = PartRef, seq = PartSeq, total = PartsTotal}},
            encoding = Encoding,
            body = Body,
            src_addr = SrcAddr,
            dst_addr = DstAddr,
            reg_dlr = RegDlr,
            esm_class = EsmClass,
            val_period = ValPeriod,
            req_time = ReqTime,
            network_id = undefined,
            price = undefined
    },
    Modifier = set_mt_req_info_modifier(ReqInfo),
    Expected = {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set', {
            'ci' , CID,
            'ui' , UID,
            'ct' , bsondoc:atom_to_binary(CT),
            'gi' , GID,
            't'  , {
                'n' , bsondoc:atom_to_binary(Type),
                'r' , PartRef,
                's' , PartSeq,
                't' , PartsTotal
            },
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        }
    },
    ?assertEqual(Expected, Modifier).

set_mt_req_info_modifier_part_test() ->
    RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
    GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
    CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
    UID = <<"user">>,
    CT = funnel,
    Body = <<"Hello">>,
    Type = part,
    Encoding = default,
    PartRef = 249,
    PartSeq = 3,
    PartsTotal = 3,
    SrcAddr = #addr{},
    DstAddr = #addr{},
    RegDlr = false,
    EsmClass = 0,
    ValPeriod = <<"000003000000000R">>,
    ReqTime = {0,0,0},
    NID = <<"390a8f74-a60e-11e4-a113-28d2445f2979">>,
    Price = 1.0,
    ReqInfo = #req_info{
            req_id = RID,
            customer_id = CID,
            user_id = UID,
            client_type = CT,
            in_msg_id = <<"3">>,
            gateway_id = GID,
            type = {part, #part_info{ref = PartRef, seq = PartSeq, total = PartsTotal}},
            encoding = Encoding,
            body = Body,
            src_addr = SrcAddr,
            dst_addr = DstAddr,
            reg_dlr = RegDlr,
            esm_class = EsmClass,
            val_period = ValPeriod,
            req_time = ReqTime,
            network_id = NID,
            price = Price
    },
    Modifier = set_mt_req_info_modifier(ReqInfo),
    Expected = {
        '$setOnInsert', {
            's'  , <<"pending">>,
            'omi', ?UNKNOWN_ID,
            'rpt', ?UNKNOWN_TIME,
            'dt' , ?UNKNOWN_TIME
        },
        '$set', {
            'ci' , CID,
            'ui' , UID,
            'ct' , bsondoc:atom_to_binary(CT),
            'gi' , GID,
            't'  , {
                'n' , bsondoc:atom_to_binary(Type),
                'r' , PartRef,
                's' , PartSeq,
                't' , PartsTotal
            },
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'da' , k_storage_utils:addr_to_doc(DstAddr),
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime,
            'ni' , NID,
            'p'  , Price
        }
    },
    ?assertEqual(Expected, Modifier).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
