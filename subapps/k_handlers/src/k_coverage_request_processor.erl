-module(k_coverage_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/network.hrl").
-include_lib("k_storage/include/network_map.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #k1api_coverage_request_dto{}) ->
    ReqId      = Req#k1api_coverage_request_dto.id,
    CustomerId = Req#k1api_coverage_request_dto.customer_id,
    _UserId    = Req#k1api_coverage_request_dto.user_id,
    _Version   = Req#k1api_coverage_request_dto.version,

    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            NetworkMapId = Customer#customer.network_map_id,
            case k_storage_network_maps:get_network_map(NetworkMapId) of
                {ok, NetworkMap} ->
                    NetworkIds = NetworkMap#network_map.network_ids,
                    Networks = get_networks(NetworkIds),
                    Providers = get_providers(Networks),
                    NetworksDTO = [network_to_dto(N) || N <- Networks],
                    ProvidersDTO = [provider_to_dto(P) || P <- Providers],
                    DefaultProviderId = Customer#customer.default_provider_id,
                    {ok, #k1api_coverage_response_dto{
                        id = ReqId,
                        networks = NetworksDTO,
                        providers = ProvidersDTO,
                        default_provider_id = DefaultProviderId
                    }};
                {error, Error} ->
                    ?log_error("Get map id: ~p failed with: ~p", [NetworkMapId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?log_error("Get customer id: ~p failed with: ~p", [CustomerId, Error]),
            {error, Error}
    end;
process(Req = #coverage_req_v1{}) ->
    ReqId      = Req#coverage_req_v1.req_id,
    CustomerId = Req#coverage_req_v1.customer_id,

    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            NetworkMapId = Customer#customer.network_map_id,
            case k_storage_network_maps:get_network_map(NetworkMapId) of
                {ok, NetworkMap} ->
                    NetworkIds = NetworkMap#network_map.network_ids,
                    Networks = get_networks(NetworkIds),
                    Providers = get_providers(Networks),
                    Networks2 = [network_to_v1(N) || N <- Networks],
                    Providers2 = [provider_to_v1(P) || P <- Providers],
                    DefaultProviderId = Customer#customer.default_provider_id,
                    {ok, #coverage_resp_v1{
                        req_id = ReqId,
                        networks = Networks2,
                        providers = Providers2,
                        default_provider_id = DefaultProviderId
                    }};
                {error, Error} ->
                    ?log_error("Get map id: ~p failed with: ~p", [NetworkMapId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?log_error("Get customer id: ~p failed with: ~p", [CustomerId, Error]),
            {error, Error}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

get_networks(NetworkIds) ->
    get_networks(NetworkIds, []).

get_networks([], Acc) ->
    Acc;
get_networks([NetworkId | NetworkIds], Acc) ->
    case k_storage_networks:get_network(NetworkId) of
        {ok, Network} ->
            get_networks(NetworkIds, [Network | Acc]);
        {error, Error} ->
            ?log_error("Get network id: ~p failed with: ~p", [NetworkId, Error]),
            get_networks(NetworkIds, Acc)
    end.

get_providers(Networks) ->
    ProvIds = lists:usort([N#network.provider_id || N <- Networks]),
    get_providers(ProvIds, []).

get_providers([], Acc) ->
    Acc;
get_providers([ProvId | ProvIds], Acc) ->
    case k_storage_providers:get_provider(ProvId) of
        {ok, Provider} ->
            get_providers(ProvIds, [Provider | Acc]);
        {error, Error} ->
            ?log_error("Get provider id: ~p failed with: ~p", [ProvId, Error]),
            get_providers(ProvIds, Acc)
    end.

network_to_dto(Network) ->
    #network{
        id = Id,
        name = Name,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = Network,
    #network_dto{
        id = Id,
        name = Name,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

network_to_v1(Network) ->
    #network{
        id = Id,
        name = Name,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = Network,
    #network_v1{
        id = Id,
        name = Name,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

provider_to_dto(Provider) ->
    #provider{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    } = Provider,
    #provider_dto{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    }.

provider_to_v1(Provider) ->
    #provider{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    } = Provider,
    #provider_v1{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    }.
