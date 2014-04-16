-module(k_http_api_handler_gateways).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/gateway.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = id, mandatory = false, repeated = false, type = binary}
    ],

    Update = [
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = false, repeated = false, type = binary},
        #param{name = rps, mandatory = false, repeated = false, type = integer}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = rps, mandatory = true, repeated = false, type = integer}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/gateways/[:id]"
    }}.

read(Params) ->
    Uuid = ?gv(id, Params),
    case Uuid of
        undefined -> read_all();
        _ -> read_id(Uuid)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            Uuid = uuid:unparse(uuid:generate()),
            create_gtw(lists:keyreplace(id, 1, Params, {id, Uuid}));
        _Value ->
            is_exist(Params)
    end.

update(Params) ->
    Uuid = ?gv(id, Params),
    case k_storage_gateways:get_gateway(Uuid) of
        {ok, Gtw = #gateway{}} ->
            update_gtw(Gtw, Params);
        {error, no_entry} ->
            {exception, 'svc0004'}
    end.

delete(Params) ->
    Uuid = ?gv(id, Params),
    k_snmp:delete_gateway(Uuid),
    ok = k_storage_gateways:del_gateway(Uuid),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
    case k_storage_gateways:get_gateways() of
        {ok, GtwList} ->
            {ok, GtwPropLists} = prepare_gtws(GtwList),
            ?log_debug("GtwPropLists: ~p", [GtwPropLists]),
            {ok, {gateways, GtwPropLists}};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

read_id(GtwUuid) ->
    case k_storage_gateways:get_gateway(GtwUuid) of
        {ok, Gtw = #gateway{}} ->
            {ok, [GtwPropList]} = prepare_gtws([{GtwUuid, Gtw}]),
            ?log_debug("GtwPropList: ~p", [GtwPropList]),
            {ok, GtwPropList};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

is_exist(Params) ->
    Uuid = ?gv(id, Params),
    case k_storage_gateways:get_gateway(Uuid) of
        {ok, #gateway{}} ->
            ?log_warn("Gateway already exist. Abort.", []),
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_gtw(Params)
    end.

update_gtw(Gtw, Params) ->
    Uuid = ?gv(id, Params),
    #gateway{rps = RPS, name = Name, connections = Conns} = Gtw,
    NewRPS = ?gv(rps, Params, RPS),
    ?log_debug("NewRPS: ~p", [NewRPS]),
    NewName = ?gv(name, Params, Name),
    NewGtw = #gateway{rps = NewRPS, name = NewName, connections = Conns},
    ?log_debug("New gtw: ~p", [NewGtw]),
    k_snmp:set_gateway(Uuid, NewName, NewRPS),
    ok = k_storage_gateways:set_gateway(Uuid, NewGtw),
    case k_storage_gateways:get_gateway(Uuid) of
        {ok, NewGtw = #gateway{}} ->
            ?log_debug("NewGtw: ~p", [NewGtw]),
            {ok, [GtwPropList]} = prepare_gtws([{Uuid, NewGtw}]),
            ?log_debug("GtwPropList: ~p", [GtwPropList]),
            {http_code, 200, GtwPropList};
        {error, no_entry} ->
            ?log_warn("Gateway not found after creation [~p]", [Uuid]),
            {http_code, 500};
        Any ->
            ?log_error("Unexpected error: ~p", [Any]),
            {http_code, 500}
    end.

create_gtw(Params) ->
    RPS = ?gv(rps, Params),
    Name = ?gv(name, Params),
    Uuid = ?gv(id, Params),
    Gateway = #gateway{rps = RPS, name = Name},
    k_snmp:set_gateway(Uuid, Name, RPS),
    ok = k_storage_gateways:set_gateway(Uuid, Gateway),
    case k_storage_gateways:get_gateway(Uuid) of
        {ok, Gtw = #gateway{}} ->
            {ok, [GtwPropList]} = prepare_gtws([{Uuid, Gtw}]),
            ?log_debug("GtwPropList: ~p", [GtwPropList]),
            {http_code, 201, GtwPropList};
        {error, no_entry} ->
            ?log_warn("Gateway not found after creation [~p]", [Uuid]),
            {http_code, 500};
        Any ->
            ?log_error("Unexpected error: ~p", [Any]),
            {http_code, 500}
    end.

prepare_gtws(GtwList) when is_list(GtwList) ->
    prepare_gtws(GtwList, []);
prepare_gtws(Gtw = {_Uuid, #gateway{}}) ->
    prepare_gtws([Gtw], []).

prepare_gtws([], Acc) ->
    {ok, Acc};
prepare_gtws([{GtwUuid, Gtw = #gateway{connections = Conns}} | Rest], Acc) ->
    %% convert connections records to proplists
    ConnFun = ?record_to_proplist(connection),
    ConnPropLists = [ConnFun(ConnRec) || ConnRec <- Conns],
    %% convert gateway record to proplist
    GatewayFun = ?record_to_proplist(gateway),
    GtwPropList =  [{id, GtwUuid}] ++
        GatewayFun(Gtw#gateway{connections = ConnPropLists}),
    prepare_gtws(Rest, [GtwPropList | Acc]).
