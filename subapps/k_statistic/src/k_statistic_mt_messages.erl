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
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),
    OrderBy = decode_order_by(?gv(order_by, Params)),
    OrderDirection = decode_order_direction(?gv(order_direction, Params)),
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
        {'$query',
            bson:document(
                [{'rqt', {'$gte', From, '$lt', To}}] ++
                CustomerSelector ++
                RecipientSelector ++
                StatusSelector
            ),
         '$orderby', {OrderBy, OrderDirection}
        },
    {ok, Docs} = shifted_storage:find(mt_messages, Selector, {}, Skip, Limit),
    [k_statistic_utils:doc_to_mt_msg(Doc) || {_, Doc} <- Docs].

-spec build_msg_report(msg_id()) -> [[{atom(), term()}]].
build_msg_report(MsgId) ->
    Selector = {
        '_id', k_storage_utils:binary_to_objectid(MsgId)
    },
    {ok, Doc} = shifted_storage:find_one(mt_messages, Selector),
    [k_statistic_utils:doc_to_mt_msg(Doc)].

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
    Command = {
        'aggregate', <<"mt_messages">>,
        'pipeline', [
            {'$match', {
                'rqt', {'$gte', From, '$lt', To},
                'ci', CustomerSelector
            }},
           group(GroupBy),
           project(GroupBy)
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    SortedDocs = lists:sort(Docs),
    [build_mt_aggr_report_response(Doc) || Doc <- SortedDocs].

-spec build_aggr_recipient_report() -> {ok, [tuple()]}.
build_aggr_recipient_report() ->
    Command = {
        'aggregate', <<"mt_messages">>,
        'pipeline', [
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
        ]
    },
    {ok, _Docs} = shifted_storage:command(Command).

%% ===================================================================
%% Internals
%% ===================================================================

group(hourly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'hour', {'$hour', <<"$rqt">>},
            'customer_id', <<"$ci">>
        },
        'messages' , {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'customer_id', <<"$ci">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'customer_id', <<"$ci">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }}.

project(hourly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'hour', <<"$_id.hour">>,
        'customer_id', <<"$_id.customer_id">>,
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'customer_id', <<"$_id.customer_id">>,
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'customer_id', <<"$_id.customer_id">>,
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

build_mt_aggr_report_response(Doc) ->
    Y = bsondoc:at(year, Doc),
    M = bsondoc:at(month, Doc),
    D = bsondoc:at(day, Doc, 1),
    H = bsondoc:at(hour, Doc, 0),
    Date = ac_datetime:datetime_to_iso8601({{Y,M,D},{H,0,0}}),
    Doc2 = bson:exclude([year, month, day, hour], Doc),
    Doc3 = bson:update(date, Date, Doc2),
    bson:fields(Doc3).

decode_order_by(<<"req_time">>) ->
    rqt;
decode_order_by(<<"status">>) ->
    s;
decode_order_by(<<"customer_name">>) ->
    rqt;
decode_order_by(<<"user_id">>) ->
    rqt;
decode_order_by(<<"client_type">>) ->
    ct;
decode_order_by(<<"dst_addr.addr">>) ->
    'da.a';
decode_order_by(<<"src_addr.addr">>) ->
    'sa.a'.

decode_order_direction(<<"asc">>) ->
    1;
decode_order_direction(<<"desc">>) ->
    -1.
