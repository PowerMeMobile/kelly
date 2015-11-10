-module(k_http_api_handler_providers).

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
-include_lib("k_storage/include/provider.hrl").

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
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = gateway_id, mandatory = false, repeated = false, type = binary},
        #param{name = bulk_gateway_id, mandatory = false, repeated = false, type = binary},
        #param{name = receipts_supported, mandatory = false, repeated = false, type = boolean},
        #param{name = sms_add_points, mandatory = false, repeated = false, type = float}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = bulk_gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = receipts_supported, mandatory = true, repeated = false, type = boolean},
        #param{name = sms_add_points, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/providers/[:id]"
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
            Uuid = uuid:unparse(uuid:generate_time()),
            create_provider(lists:keyreplace(id, 1, Params, {id, Uuid}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    case k_storage_providers:get_provider(?gv(id, Params)) of
        {ok, Provider = #provider{}} ->
            update_provider(Provider, Params);
        {error, no_entry} ->
            {exception, 'svc0003'};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

delete(Params) ->
    Id = ?gv(id, Params),
    case k_storage_providers:can_del_provider(Id) of
        true ->
            ok = k_storage_providers:del_provider(Id),
            {http_code, 204};
        false ->
            {http_code, 403}
    end.


%% ===================================================================
%% Internal
%% ===================================================================

is_exist(Params) ->
    case k_storage_providers:get_provider(?gv(id, Params)) of
        {ok, #provider{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_provider(Params);
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_all() ->
    case k_storage_providers:get_providers() of
        {ok, Entries} ->
            {ok, Plists} = prepare(Entries),
            ?log_debug("Providers: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(Uuid) ->
    case k_storage_providers:get_provider(Uuid) of
        {ok, Entry = #provider{}} ->
            {ok, [Plist]} = prepare(Entry),
            ?log_debug("Provider: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003', []};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update_provider(Provider, Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params, Provider#provider.name),
    Description = ?gv(description, Params, Provider#provider.description),
    GatewayId = ?gv(gateway_id, Params, Provider#provider.gateway_id),
    BulkGatewayId = ?gv(bulk_gateway_id, Params, Provider#provider.bulk_gateway_id),
    ReceiptsSupported = ?gv(receipts_supported, Params, Provider#provider.receipts_supported),
    SmsAddPoints = ?gv(sms_add_points, Params, Provider#provider.sms_add_points),
    Updated = #provider{
        id = Id,
        name = Name,
        description = Description,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    },
    ok = k_storage_providers:set_provider(Id, Updated),
    {ok, [Plist]} = prepare(Updated),
    ?log_debug("Provider: ~p", [Plist]),
    {http_code, 200, Plist}.

create_provider(Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params),
    Description = ?gv(description, Params),
    GatewayId = ?gv(gateway_id, Params),
    BulkGatewayId = ?gv(bulk_gateway_id, Params),
    ReceiptsSupported = ?gv(receipts_supported, Params),
    SmsAddPoints = ?gv(sms_add_points, Params),
    Provider = #provider{
        id = Id,
        name = Name,
        description = Description,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    },
    ok = k_storage_providers:set_provider(Id, Provider),
    {ok, [Plist]} = prepare(Provider),
    ?log_debug("Provider: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare(List) when is_list(List) ->
    prepare(List, []);
prepare(Entry = #provider{}) ->
    prepare([Entry]).

prepare([], Acc) ->
    {ok, Acc};
prepare([Prv = #provider{} | Rest], Acc) ->
    Fun = ?record_to_proplist(provider),
    Plist = Fun(Prv),
    prepare(Rest, [Plist | Acc]).
