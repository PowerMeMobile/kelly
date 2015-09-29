-module(k_statistic_mt_aggr_reports).

-export([
    by_country/1,
    by_country_and_network/1,
    by_period/1,
    by_gateway/1,
    summary/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/network.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec by_country([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
by_country(Params) ->
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
            by_network_group(GroupBy),
            by_network_project(GroupBy)
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    ByNetResps = [response(Doc) || Doc <- Docs],
    NetIds = [proplists:get_value(network_id, R) || R <- ByNetResps],
    NetDict = k_storage_utils:get_id_to_network_dict(NetIds),
    AddCountryAndRmNetId =
        fun(R, Dict) ->
            NetId = proplists:get_value(network_id, R),
            R2 = proplists:delete(network_id, R),
            Net = dict:fetch(NetId, Dict),
            [{country, Net#network.country} | R2]
        end,
    group_by_date_and_country(modify(AddCountryAndRmNetId, ByNetResps, NetDict)).

-spec by_country_and_network([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
by_country_and_network(Params) ->
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
            by_network_group(GroupBy),
            by_network_project(GroupBy),
            {'$sort',
                {'year', 1, 'month', 1, 'day', 1, 'hour', 1}}
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    ByNetResps = [response(Doc) || Doc <- Docs],
    NetIds = [proplists:get_value(network_id, R) || R <- ByNetResps],
    NetDict = k_storage_utils:get_id_to_network_dict(NetIds),
    AddCountryAndName =
        fun(R, Dict) ->
            NetId = proplists:get_value(network_id, R),
            Net = dict:fetch(NetId, Dict),
            [{country, Net#network.country},
             {network_name, Net#network.name} | R]
        end,
    modify(AddCountryAndName, ByNetResps, NetDict).

-spec by_gateway([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
by_gateway(Params) ->
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
            by_gateway_group(GroupBy),
            by_gateway_project(GroupBy),
            {'$sort',
                {'year', 1, 'month', 1, 'day', 1, 'hour', 1}}
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    Resps = [response(Doc) || Doc <- Docs],
    GtwIds = [proplists:get_value(gateway_id, R) || R <- Resps],
    GtwDict = k_storage_utils:get_id_to_gateway_dict(GtwIds),
    AddName =
        fun(R, Dict) ->
            Id = proplists:get_value(gateway_id, R),
            Gtw = dict:fetch(Id, Dict),
            [{gateway_name, Gtw#gateway.name} | R]
        end,
    modify(AddName, Resps, GtwDict).

-spec by_period([{atom(), term()}]) -> [[{bson:label(), bson:value()}]].
by_period(Params) ->
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
            by_period_group(GroupBy),
            by_period_project(GroupBy),
            {'$sort',
                {'year', 1, 'month', 1, 'day', 1, 'hour', 1}}
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    [response(Doc) || Doc <- Docs].

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
            summary_group(GroupBy),
            summary_project(GroupBy),
            {'$sort',
                {'year', 1, 'month', 1, 'day', 1, 'hour', 1}}
        ]
    },
    {ok, Docs} = shifted_storage:command(Command),
    [response(Doc) || Doc <- Docs].

%% ===================================================================
%% Internals
%% ===================================================================

response(Doc) ->
    Y = bsondoc:at(year, Doc),
    M = bsondoc:at(month, Doc),
    D = bsondoc:at(day, Doc, 1),
    H = bsondoc:at(hour, Doc, 0),
    Date = ac_datetime:datetime_to_iso8601({{Y,M,D},{H,0,0}}),
    Doc2 = bson:exclude([year, month, day, hour], Doc),
    Doc3 = bson:update(date, Date, Doc2),
    bson:fields(Doc3).

summary_group(hourly) ->
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
summary_group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>}
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
summary_group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>}
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }}.

summary_project(hourly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'hour', <<"$_id.hour">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
summary_project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
summary_project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

by_period_group(hourly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'hour', {'$hour', <<"$rqt">>},
            'client_type', <<"$ct">>
        },
        'messages' , {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_period_group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'client_type', <<"$ct">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_period_group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'client_type', <<"$ct">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }}.

by_period_project(hourly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'hour', <<"$_id.hour">>,
        'client_type', <<"$_id.client_type">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_period_project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'client_type', <<"$_id.client_type">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_period_project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'client_type', <<"$_id.client_type">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

by_gateway_group(hourly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'hour', {'$hour', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'gateway_id', <<"$gi">>
        },
        'messages' , {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_gateway_group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'gateway_id', <<"$gi">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_gateway_group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'gateway_id', <<"$gi">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }}.

by_gateway_project(hourly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'hour', <<"$_id.hour">>,
        'client_type', <<"$_id.client_type">>,
        'gateway_id', <<"$_id.gateway_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_gateway_project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'client_type', <<"$_id.client_type">>,
        'gateway_id', <<"$_id.gateway_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_gateway_project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'client_type', <<"$_id.client_type">>,
        'gateway_id', <<"$_id.gateway_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

by_network_group(hourly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'hour', {'$hour', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'network_id', <<"$ni">>
        },
        'messages' , {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_network_group(daily) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'day', {'$dayOfMonth', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'network_id', <<"$ni">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }};
by_network_group(monthly) ->
    {'$group', {
        '_id', {
            'year', {'$year', <<"$rqt">>},
            'month', {'$month', <<"$rqt">>},
            'client_type', <<"$ct">>,
            'network_id', <<"$ni">>
        },
        'messages', {'$sum', 1},
        'revenue', {'$sum', <<"$p">>}
    }}.

by_network_project(hourly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'hour', <<"$_id.hour">>,
        'client_type', <<"$_id.client_type">>,
        'network_id', <<"$_id.network_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_network_project(daily) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'day', <<"$_id.day">>,
        'client_type', <<"$_id.client_type">>,
        'network_id', <<"$_id.network_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }};
by_network_project(monthly) ->
    {'$project', {
        '_id', 0,
        'year', <<"$_id.year">>,
        'month', <<"$_id.month">>,
        'client_type', <<"$_id.client_type">>,
        'network_id', <<"$_id.network_id">>,
        'messages', <<"$messages">>,
        'revenue', <<"$revenue">>
    }}.

modify(Fun_2, Rs, Dict) ->
    modify(Fun_2, Rs, Dict, []).

modify(_Fun_2, [], _Dict, Acc) ->
    lists:reverse(Acc);
modify(Fun_2, [R | Rs], Dict, Acc) ->
    R2 = Fun_2(R, Dict),
    modify(Fun_2, Rs, Dict, [R2 | Acc]).

group_by_date_and_country(Rs) ->
    group_by_date_and_country(Rs, dict:new()).

group_by_date_and_country([], D) ->
    L = dict:to_list(D),
    L2 = lists:keysort(1, L),
    [[{date, Dt},
      {country, C},
      {client_type, Ct},
      {messages, N},
      {revenue, R}] || {{Dt, C, Ct}, {N, R}} <- L2];
group_by_date_and_country([R | Rs], D) ->
    Date = proplists:get_value(date, R),
    Country = proplists:get_value(country, R),
    ClientType = proplists:get_value(client_type, R),
    Messages = proplists:get_value(messages, R),
    Revenue = proplists:get_value(revenue, R),
    Update = fun({Msgs, Revs}) -> {Messages + Msgs, Revenue + Revs} end,
    D2 = dict:update({Date, Country, ClientType}, Update, {Messages, Revenue}, D),
    group_by_date_and_country(Rs, D2).
