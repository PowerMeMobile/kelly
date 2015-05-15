-module(k_storage_network_maps).

%% API
-export([
    set_network_map/2,
    get_network_map/1,
    get_network_maps/0,
    del_network_map/1
]).

-include("storages.hrl").
-include("network_map.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_network_map(network_map_id(), #network_map{}) ->
    ok | {error, term()}.
set_network_map(NetworkMapId, NetworkMap)->
    Modifier = {
        '$set', {
            'name'        , NetworkMap#network_map.name,
            'network_ids' , NetworkMap#network_map.network_ids
        }
    },
    case mongodb_storage:upsert(static_storage, network_maps, {'_id', NetworkMapId}, Modifier) of
        ok ->
            ok = k_event_manager:notify_network_map_changed(NetworkMapId);
        {error, Error} ->
            {error, Error}
    end.

-spec get_network_map(network_map_id()) ->
    {ok, #network_map{}} | {error, no_entry} | {error, term()}.
get_network_map(NetworkMapId) ->
    case mongodb_storage:find_one(static_storage, network_maps, {'_id', NetworkMapId}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec get_network_maps() ->
    {ok, [#network_map{}]} | {error, term()}.
get_network_maps() ->
    case mongodb_storage:find(static_storage, network_maps, {}) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        Error ->
            Error
    end.

-spec del_network_map(network_map_id()) ->
    ok | {error, no_entry} | {error, term()}.
del_network_map(NetworkMapId) ->
    case mongodb_storage:delete(static_storage, network_maps, {'_id', NetworkMapId}) of
        ok ->
            ok = k_event_manager:notify_network_map_changed(NetworkMapId);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    Id = bsondoc:at('_id', Doc),
    Name = bsondoc:at(name, Doc),
    NetworkIds = bsondoc:at(network_ids, Doc),
    #network_map{
        id = Id,
        name = Name,
        network_ids = NetworkIds
    }.
