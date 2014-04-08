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
-include_lib("k_common/include/logging.hrl").
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
        #param{name = gateway_id, mandatory = false, repeated = false, type = binary},
        #param{name = bulk_gateway_id, mandatory = false, repeated = false, type = binary},
        #param{name = receipts_supported, mandatory = false, repeated = false, type = boolean},
        #param{name = sms_add_credits, mandatory = false, repeated = false, type = float}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = bulk_gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = receipts_supported, mandatory = true, repeated = false, type = boolean},
        #param{name = sms_add_credits, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/providers/[:id]"
    }}.

read(Params) ->
    UUID = ?gv(id, Params),
    case UUID of
        undefined -> read_all();
        _ -> read_id(UUID)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            UUID = uuid:unparse(uuid:generate_time()),
            create_provider(lists:keyreplace(id, 1, Params, {id, UUID}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    case k_config:get_provider(?gv(id, Params)) of
        {ok, Provider = #provider{}} ->
            update_provider(Provider, Params);
        {error, no_entry} ->
            {exception, 'svc0003'};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

delete(Params) ->
    ok = k_config:del_provider(?gv(id, Params)),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

is_exist(Params) ->
    case k_config:get_provider(?gv(id, Params)) of
        {ok, #provider{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_provider(Params);
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_all() ->
    case k_config:get_providers() of
        {ok, PrvList} ->
            {ok, PrvPropLists} = prepare(PrvList),
            ?log_debug("PrvPropLists: ~p", [PrvPropLists]),
            {http_code, 200, {providers, PrvPropLists}};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(PrvUUID) ->
    case k_config:get_provider(PrvUUID) of
        {ok, Prv = #provider{}} ->
            {ok, [PrvPropList]} = prepare({PrvUUID, Prv}),
            ?log_debug("PrvPropList: ~p", [PrvPropList]),
            {http_code, 200, PrvPropList};
        {error, no_entry} ->
            {exception, 'svc0003', []};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update_provider(Provider, Params) ->
    ID = ?gv(id, Params),
    Name = ?gv(name, Params, Provider#provider.name),
    GatewayId = ?gv(gateway_id, Params, Provider#provider.gateway_id),
    BulkGatewayId = ?gv(bulk_gateway_id, Params, Provider#provider.bulk_gateway_id),
    ReceiptsSupported = ?gv(receipts_supported, Params, Provider#provider.receipts_supported),
    SmsAddCredits = ?gv(sms_add_credits, Params, Provider#provider.sms_add_credits),
    Updated = #provider{
        name = Name,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_credits = SmsAddCredits
    },
    ok = k_config:set_provider(ID, Updated),
    {ok, [PrvPropList]} = prepare({ID, Updated}),
    ?log_debug("PrvPropList: ~p", [PrvPropList]),
    {http_code, 200, PrvPropList}.

create_provider(Params) ->
    UUID = ?gv(id, Params),
    Name = ?gv(name, Params),
    GatewayId = ?gv(gateway_id, Params),
    BulkGatewayId = ?gv(bulk_gateway_id, Params),
    ReceiptsSupported = ?gv(receipts_supported, Params),
    SmsAddCredits = ?gv(sms_add_credits, Params),
    Provider = #provider{
        name = Name,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_credits = SmsAddCredits
    },
    ok = k_config:set_provider(UUID, Provider),
    {ok, [PropList]} = prepare({UUID, Provider}),
    ?log_debug("PropList: ~p", [PropList]),
    {http_code, 201, PropList}.

prepare(PrvList) when is_list(PrvList) ->
    prepare(PrvList, []);
prepare(Prv = {_UUID, #provider{}}) ->
    prepare([Prv]).

prepare([], Acc) ->
    {ok, Acc};
prepare([{PrvUUID, Prv = #provider{}} | Rest], Acc) ->
    PrvFun = ?record_to_proplist(provider),
    PropList = PrvFun(Prv),
    Name = ?gv(name, PropList),
    GatewayId = ?gv(gateway_id, PropList),
    BulkGatewayId = ?gv(bulk_gateway_id, PropList),
    ReceiptsSupported = ?gv(receipts_supported, PropList),
    SmsAddCredits = ?gv(sms_add_credits, PropList),
    Result = [
        {id, PrvUUID},
        {name, Name},
        {gateway_id, GatewayId},
        {bulk_gateway_id, BulkGatewayId},
        {receipts_supported, ReceiptsSupported},
        {sms_add_credits, SmsAddCredits}
    ],
    prepare(Rest, [Result | Acc]).
