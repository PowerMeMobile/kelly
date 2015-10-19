-module(k_handlers_utils).

-export([
    get_networks_and_providers/2,
    network_to_v1/1,
    provider_to_v1/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_storage/include/network.hrl").
-include_lib("k_storage/include/network_map.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_networks_and_providers(network_map_id(), undefined | provider_id()) ->
    {ok, [#network{}], [#provider{}]}.
get_networks_and_providers(NetMapId, DefProvId) ->
    Key = {NetMapId, DefProvId},
    case k_handlers_auth_cache:get(Key) of
        {ok, {Ns, Ps}} ->
            {ok, Ns, Ps};
        {error, no_entry} ->
            {Ns, Ps} = build_networks_and_providers(NetMapId, DefProvId),
            k_handlers_auth_cache:set(Key, {Ns, Ps}),
            {ok, Ns, Ps}
    end.

-spec network_to_v1(#network{}) -> #network_v1{}.
network_to_v1(N) ->
    #network{
        id = Id,
        name = Name,
        country_code = CC,
        number_len = NL,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = N,
    #network_v1{
        id = Id,
        name = Name,
        country_code = CC,
        %% now number_len in db without CC length or zero
        number_len = if NL =:= 0 -> 0; true -> NL + erlang:size(CC) end,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

-spec provider_to_v1(#provider{}) -> #provider_v1{}.
provider_to_v1(P) ->
    #provider{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    } = P,
    #provider_v1{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    }.

%% ===================================================================
%% Internal
%% ===================================================================

build_networks_and_providers(NetMapId, undefined) ->
    case k_storage_network_maps:get_network_map(NetMapId) of
        {ok, #network_map{network_ids = NetIds}} ->
            {Networks, Providers} = lists:foldl(fun(NetId, {Ns, Ps})->
                case k_storage_networks:get_network(NetId) of
                    {ok, N} ->
                        ProvId = N#network.provider_id,
                        case lists:keyfind(ProvId, #provider.id, Ps) of
                            false ->
                                case k_storage_providers:get_provider(ProvId) of
                                    {ok, P} ->
                                        {[N | Ns], [P | Ps]};
                                    {error, Error} ->
                                        ?log_error(
                                            "Get provider id: ~p from network id: ~p from map id: ~p failed with: ~p",
                                            [ProvId, NetId, NetMapId, Error]),
                                        error(storage_error)
                                end;
                            _Found ->
                                {[N | Ns], Ps}
                        end;
                    {error, Error} ->
                        ?log_error("Get network id: ~p from map id: ~p failed with: ~p",
                            [NetId, NetMapId, Error]),
                        error(storage_error)
                end
            end, {[], []}, NetIds),
            {Networks, Providers};
        {error, Error} ->
            ?log_error("Get map id: ~p failed with: ~p", [NetMapId, Error]),
            error(storage_error)
    end;
build_networks_and_providers(NetMapId, DefProvId) ->
    case k_storage_network_maps:get_network_map(NetMapId) of
        {ok, #network_map{network_ids = NetIds}} ->
            Networks = lists:foldl(fun(NetId, Ns)->
                case k_storage_networks:get_network(NetId) of
                    {ok, N} ->
                        N2 = N#network{provider_id = DefProvId},
                        [N2 | Ns];
                    {error, Error} ->
                        ?log_error("Get network id: ~p from map id: ~p failed with: ~p",
                            [NetId, NetMapId, Error]),
                        error(storage_error)
                end
            end, [], NetIds),
            case k_storage_providers:get_provider(DefProvId) of
                {ok, DefProv} ->
                    {Networks, [DefProv]};
                {error, Error} ->
                    ?log_error("Get default provider id: ~p failed with: ~p",
                        [DefProvId, Error]),
                    error(storage_error)
            end;
        {error, Error} ->
            ?log_error("Get map id: ~p failed with: ~p", [NetMapId, Error]),
            error(storage_error)
    end.
