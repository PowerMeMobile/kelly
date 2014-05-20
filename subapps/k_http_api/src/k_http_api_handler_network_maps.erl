-module(k_http_api_handler_network_maps).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/network_map.hrl").

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
        #param{name = network_ids, mandatory = false, repeated = true, type = binary}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = network_ids, mandatory = true, repeated = true, type = binary}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/network_maps/[:id]"
    }}.

read(Params) ->
    Id = ?gv(id, Params),
    case Id of
        undefined ->
            read_all();
        _ ->
            read_id(Id)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            Id = uuid:unparse(uuid:generate_time()),
            create_network_map(lists:keyreplace(id, 1, Params, {id, Id}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    Id = ?gv(id, Params),
    case k_storage_network_maps:get_network_map(Id) of
        {ok, NtwMap = #network_map{}} ->
            update_network_map(NtwMap, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    Id = ?gv(id, Params),
    ok = k_storage_network_maps:del_network_map(Id),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
    case k_storage_network_maps:get_network_maps() of
        {ok, Entries} ->
            {ok, Plists} = prepare(Entries),
            ?log_debug("Network maps: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(Id) ->
    case k_storage_network_maps:get_network_map(Id) of
        {ok, Entry = #network_map{}} ->
            {ok, [Plist]} = prepare(Entry),
            ?log_debug("Network map: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

is_exist(Params) ->
    Id = ?gv(id, Params),
    case k_storage_network_maps:get_network_map(Id) of
        {ok, #network_map{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_network_map(Params)
    end.

update_network_map(NetworkMap, Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params, NetworkMap#network_map.name),
    NetworkIds = ?gv(network_ids, Params, NetworkMap#network_map.network_ids),
    Updated = #network_map{
        id = Id,
        name = Name,
        network_ids = NetworkIds
    },
    ok = k_storage_network_maps:set_network_map(Id, Updated),
    {ok, [Plist]} = prepare(Updated),
    ?log_debug("Network map: ~p", [Plist]),
    {http_code, 200, Plist}.

create_network_map(Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params),
    NetworkIds = ?gv(network_ids, Params),
    NetworkMap = #network_map{
        id = Id,
        name = Name,
        network_ids = NetworkIds
    },
    ok = k_storage_network_maps:set_network_map(Id, NetworkMap),
    {ok, [Plist]} = prepare(NetworkMap),
    ?log_debug("Network map: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare(List) when is_list(List) ->
    prepare(List, []);
prepare(Entry = #network_map{}) ->
    prepare([Entry]).

prepare([], Acc) ->
    {ok, Acc};
prepare([NtwMap = #network_map{} | Rest], Acc) ->
    Fun = ?record_to_proplist(network_map),
    Plist = Fun(NtwMap),
    prepare(Rest, [Plist | Acc]).
