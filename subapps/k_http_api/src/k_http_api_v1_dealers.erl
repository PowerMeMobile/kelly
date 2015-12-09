-module(k_http_api_v1_dealers).

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
-include_lib("k_storage/include/dealer.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = id, mandatory = false, repeated = false, type = uuid}
    ],
    Update = [
        #param{name = id, mandatory = true, repeated = false, type = uuid},
        #param{name = name, mandatory = false, repeated = false, type = binary},
        #param{name = network_map_id, mandatory = false, repeated = false, type = uuid},
        %% the type should be uuid, but it's not clear how to allow empty uuid
        #param{name = default_provider_id, mandatory = false, repeated = false, type = binary},
        #param{name = interfaces, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun k_http_api_v1_customers:decode_state/1}}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = uuid}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = uuid},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = network_map_id, mandatory = true, repeated = false, type = uuid},
        %% the type should be uuid, but it's not clear how to allow empty uuid
        #param{name = default_provider_id, mandatory = true, repeated = false, type = binary},
        #param{name = interfaces, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun k_http_api_v1_customers:decode_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/dealers/[:id]"
    }}.


create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            DealerUuid = uuid:unparse(uuid:generate_time()),
            create_dealer(lists:keyreplace(id, 1, Params, {id, DealerUuid}));
        _ ->
            case does_exist_by_uuid(Params) of
                true ->
                    {exception, 'svc0004'};
                false ->
                    create_dealer(Params)
            end
    end.


does_exist_by_uuid(Params) ->
    DealerUuid = ?gv(id, Params),
    case k_storage_dealers:get_dealer_by_uuid(DealerUuid) of
        {ok, #dealer_v1{}} ->
            true;
        {error, no_entry} ->
            false
    end.


read(Params) ->
    case  ?gv(id, Params) of
        undefined ->
            read_all();
        DealerUuid ->
            read_dealer_uuid(DealerUuid)
    end.


read_all() ->
    case k_storage_dealers:get_dealers() of
        {ok, Dealers} ->
            {ok, Plists} = prepare_dealers(Dealers),
            ?log_debug("Dealers: ~p", [Plists]),
            {http_code, 200, Plists}
    end.


read_dealer_uuid(DealerUuid) ->
    case k_storage_dealers:get_dealer_by_uuid(DealerUuid) of
        {ok, Dealer = #dealer_v1{}} ->
            {ok, [Plist]} = prepare_dealers(Dealer),
            ?log_debug("Dealer: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update(Params) ->
    DealerUuid = ?gv(id, Params),
    case k_storage_dealers:get_dealer_by_uuid(DealerUuid) of
        {ok, Dealer = #dealer_v1{}} ->
            update_dealer(Dealer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    DealerUuid = ?gv(id, Params),
    ok = k_storage_dealers:del_dealer(DealerUuid),
    {http_code, 204}.

%% ===================================================================
%% Internal
%% ===================================================================

update_dealer(Dealer, Params) ->
    DealerUuid = Dealer#dealer_v1.id,
    NewName = ?gv(name, Params, Dealer#dealer_v1.name),
    NewNetworkMapId = ?gv(network_map_id, Params, Dealer#dealer_v1.network_map_id),
    NewDefaultProviderId = ?gv(default_provider_id, Params, Dealer#dealer_v1.default_provider_id),
    NewState = ?gv(state, Params, Dealer#dealer_v1.state),

    PreIfs = Dealer#dealer_v1.interfaces,
    NewIfs = ?gv(interfaces, Params, PreIfs),

    PreFs = Dealer#dealer_v1.features,
    NewFs = ?gv(features, Params, PreFs),

    NewDealer = #dealer_v1{
        id = DealerUuid,
        name = NewName,
        network_map_id = NewNetworkMapId,
        default_provider_id = NewDefaultProviderId,
        interfaces = NewIfs,
        features = NewFs,
        state = NewState
    },

    ok = k_storage_dealers:set_dealer(DealerUuid, NewDealer),
    {ok, [Plist]} = prepare_dealers(NewDealer),
    ?log_debug("Dealer: ~p", [Plist]),
    {http_code, 200, Plist}.

create_dealer(Params) ->
    DealerUuid = ?gv(id, Params),
    Dealer = #dealer_v1{
        id = DealerUuid,
        name = ?gv(name, Params),
        network_map_id = ?gv(network_map_id, Params),
        default_provider_id = ?gv(default_provider_id, Params),
        interfaces = ?gv(interfaces, Params),
        features = ?gv(features, Params),
        state = ?gv(state, Params)
    },
    ok = k_storage_dealers:set_dealer(DealerUuid, Dealer),
    {ok, [Plist]} = prepare_dealers(Dealer),
    ?log_debug("Dealer: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare_dealers(ItemList) when is_list(ItemList) ->
    prepare_dealers(ItemList, []);
prepare_dealers(Item) ->
    prepare_dealers([Item], []).

prepare_dealers([], Acc) ->
    {ok, Acc};
prepare_dealers([Dealer = #dealer_v1{} | Rest], Acc) ->
     #dealer_v1{
        features = Features,
        network_map_id = NetworkMapId,
        default_provider_id = DefaultProviderId
    } = Dealer,

    {ok, FeaturesPlists} =
        k_http_api_utils:prepare_features(Features),

    Fun = ?record_to_proplist(dealer_v1),
    Plist0 = Fun(
        Dealer#dealer_v1{
            features = FeaturesPlists
        }
    ),

    NetworkMapName =
    case k_storage_networks:get_network(NetworkMapId) of
        {ok, #network{name = NM}} -> NM;
        _ -> <<>>
    end,

    ProviderName =
    case k_storage_providers:get_provider(DefaultProviderId) of
        {ok, #provider{name = PN}} -> PN;
        _ -> <<>>
    end,

    Plist = [
        {network_map_name, NetworkMapName},
        {default_provider_name, ProviderName}
    | Plist0],
    prepare_dealers(Rest, [Plist | Acc]).
