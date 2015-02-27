-module(k_statistic_mt_aggr_reports).

-export([
    by_gateway/1,
    summary/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

by_gateway(Params) ->
    [].

-spec summary([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
summary(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    CustomerSel =
        case ?gv(customer_uuid, Params) of
            undefined -> [];
            CustomerUuid -> [{'ci', CustomerUuid}]
        end,
    GroupBy = ?gv(group_by, Params),
    Command = {
        'aggregate', <<"mt_messages">>,
        'pipeline', [
            {'$match',
                bson:document(
                    [{'rqt', {'$gte', From, '$lt', To}}] ++
                    CustomerSel
                )
            },
            group(GroupBy),
            project(GroupBy)
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    SortedDocs = lists:sort(Docs),
    [summary_response(Doc) || Doc <- SortedDocs].

%% ===================================================================
%% Internals
%% ===================================================================

group(hourly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'hour', {'$hour', <<"$rqt">>}
        },
        'messages' , {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>}
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>}
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
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'number', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

summary_response(Doc) ->
    Y = bsondoc:at(year, Doc),
    M = bsondoc:at(month, Doc),
    D = bsondoc:at(day, Doc, 1),
    H = bsondoc:at(hour, Doc, 0),
    Date = ac_datetime:datetime_to_iso8601({{Y,M,D},{H,0,0}}),
    Doc2 = bson:exclude([year, month, day, hour], Doc),
    Doc3 = bson:update(date, Date, Doc2),
    bson:fields(Doc3).
