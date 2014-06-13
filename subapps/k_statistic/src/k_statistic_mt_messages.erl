-module(k_statistic_mt_messages).

-export([
    build_report/1,
    build_msg_report/1,
    build_aggr_report/1,
    build_aggr_recipient_report/0
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec build_report([{atom(), term()}]) -> [[{atom(), term()}]].
build_report(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    CustomerSelector =
        case ?gv(customer_id, Params) of
            undefined -> [];
            CustomerID -> [{'ci', CustomerID}]
        end,
    RecipientSelector =
        case ?gv(recipient, Params) of
            undefined -> [];
            Recipient -> [{'da.a', Recipient}]
        end,
    StatusSelector =
        case ?gv(status, Params) of
            undefined -> [];
            Status -> [{'s', Status}]
        end,

    Selector =
        [{'rqt', {'$gte', From, '$lt', To}}] ++
        CustomerSelector ++
        RecipientSelector ++
        StatusSelector,
    {ok, Docs} = shifted_storage:find(mt_messages, bson:document(Selector)),
    [build_mt_report_response(Doc) || {_, Doc} <- Docs].

-spec build_msg_report(msg_id()) -> [[{atom(), term()}]].
build_msg_report(MsgId) ->
    Selector = {
        '_id', k_storage_utils:binary_to_objectid(MsgId)
    },
    {ok, Docs} = shifted_storage:find(mt_messages, Selector),
    [build_mt_report_response(Doc) || {_, Doc} <- Docs].

-spec build_aggr_report([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
build_aggr_report(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    CustomerSelector =
    case ?gv(customer_id, Params) of
        undefined -> {'$exists', 1};
        CustomerID -> CustomerID
    end,
    GroupBy = ?gv(group_by, Params),
    Command = {'aggregate', <<"mt_messages">>, 'pipeline', [
        {'$match', {
            'rqt', {'$gte', From, '$lt', To},
            'ci', CustomerSelector
        }},
        group(GroupBy),
        project(GroupBy)
    ]},
    {ok, Docs} = shifted_storage:command(Command),
    SortedDocs = lists:sort(Docs),
    [bson:fields(Doc) || Doc <- SortedDocs].

-spec build_aggr_recipient_report() -> {ok, [tuple()]}.
build_aggr_recipient_report() ->
    Command = {'aggregate', <<"mt_messages">>, 'pipeline', [
        {'$project', {
            '_id', 0,
            'recipient', <<"$da.a">>
        }},
        {'$group', {
            '_id', <<"$recipient">>,
            'count', {<<"$sum">>, 1}
        }},
        {'$sort', {'count', 1}},
        {'$project', {
            '_id', 0,
            'recipient', <<"$_id">>,
            'count', 1
        }}
    ]},
    {ok, _Docs} = shifted_storage:command(Command).

%% ===================================================================
%% Internals
%% ===================================================================

group(hourly) ->
    { '$group' , {
        '_id' , {
            'year' , { '$year' , <<"$rqt">> },
            'month' , { '$month' , <<"$rqt">> },
            'day' , { '$dayOfMonth' , <<"$rqt">> },
            'hour' , { '$hour' , <<"$rqt">> },
            'customer_id' , <<"$ci">> },
        'sum' , { '$sum' , 1 } } };
group(daily) ->
    { '$group' , {
        '_id' , {
            'year' , { '$year' , <<"$rqt">> },
            'month' , { '$month' , <<"$rqt">> },
            'day' , { '$dayOfMonth' , <<"$rqt">> },
            'customer_id' , <<"$ci">> },
        'sum' , { '$sum' , 1 } } };
group(monthly) ->
    { '$group' , {
        '_id' , {
            'year' , { '$year' , <<"$rqt">> },
            'month' , { '$month' , <<"$rqt">> },
            'customer_id' , <<"$ci">> },
        'sum' , { '$sum' , 1 } } }.


project(hourly) ->
    {'$project' , {
        '_id' , 0,
        'year' , <<"$_id.year">>,
        'month' , <<"$_id.month">>,
        'day' , <<"$_id.day">>,
        'hour' , <<"$_id.hour">>,
        'customer_id' , <<"$_id.customer_id">>,
        'number' , <<"$sum">> } };
project(daily) ->
    {'$project' , {
        '_id' , 0,
        'year' , <<"$_id.year">>,
        'month' , <<"$_id.month">>,
        'day' , <<"$_id.day">>,
        'customer_id' , <<"$_id.customer_id">>,
        'number' , <<"$sum">> } };
project(monthly) ->
    {'$project' , {
        '_id' , 0,
        'year' , <<"$_id.year">>,
        'month' , <<"$_id.month">>,
        'customer_id' , <<"$_id.customer_id">>,
        'number' , <<"$sum">> } }.


build_mt_report_response(Doc) ->
    MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
    Type = get_type(MsgInfo#msg_info.type),
    PartInfo = get_part_info(MsgInfo#msg_info.type),
    ReqTime  = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
    RespTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.resp_time),
    DlrTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.dlr_time),
    StatusTime = max(ReqTime, max(RespTime, DlrTime)),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    StatusISO = ac_datetime:datetime_to_iso8601(StatusTime),
    [
        {msg_id, MsgInfo#msg_info.msg_id},
        {client_type, MsgInfo#msg_info.client_type},
        {customer_id, MsgInfo#msg_info.customer_id},
        {user_id, MsgInfo#msg_info.user_id},
        {in_msg_id, MsgInfo#msg_info.in_msg_id},
        {gateway_id, MsgInfo#msg_info.gateway_id},
        {out_msg_id, MsgInfo#msg_info.out_msg_id},
        {type, Type},
        {part_info, PartInfo},
        {encoding, MsgInfo#msg_info.encoding},
        {body, MsgInfo#msg_info.body},
        {src_addr, addr_to_proplist(MsgInfo#msg_info.src_addr)},
        {dst_addr, addr_to_proplist(MsgInfo#msg_info.dst_addr)},
        {reg_dlr, MsgInfo#msg_info.reg_dlr},
        {esm_class, MsgInfo#msg_info.esm_class},
        {validity_period, MsgInfo#msg_info.val_period},
        {req_time, ReqISO},
        {status, MsgInfo#msg_info.status},
        {status_update_time, StatusISO}
    ].

addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi}
    ];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi},
        {ref_num, RefNum}
    ].

get_type(regular) ->
    regular;
get_type({part, #part_info{}}) ->
    part.

get_part_info(regular) ->
    undefined;
get_part_info({part, #part_info{
    ref = PartRef,
    seq = PartSeq,
    total = TotalParts
}}) -> [
    {ref, PartRef},
    {seq, PartSeq},
    {total, TotalParts}
].
