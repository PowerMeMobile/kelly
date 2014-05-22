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

-spec process(#k1api_coverage_request_dto{}) ->
    {ok, #k1api_coverage_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId      = ReqDTO#k1api_coverage_request_dto.id,
    CustomerId = ReqDTO#k1api_coverage_request_dto.customer_id,
    _UserId    = ReqDTO#k1api_coverage_request_dto.user_id,
    _Version   = ReqDTO#k1api_coverage_request_dto.version,
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            NetworkMapId = Customer#customer.network_map_id,
            case k_storage_network_maps:get_network_map(NetworkMapId) of
                {ok, NetworkMap} ->
                    NetworkIds = NetworkMap#network_map.network_ids,
                    case get_networks(NetworkIds) of
                        {ok, Networks} ->
                            case get_providers(Networks) of
                                {ok, Providers} ->
                                    NetworksDTO =
                                        [network_to_dto(N) || N <- Networks],
                                    ProvidersDTO =
                                        [provider_to_dto(P) || P <- Providers],
                                    DefaultProviderId = Customer#customer.default_provider_id,
                                    {ok, #k1api_coverage_response_dto{
                                        id = ReqId,
                                        networks = NetworksDTO,
                                        providers = ProvidersDTO,
                                        default_provider_id = DefaultProviderId
                                    }};
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================

get_networks(NetworkIds) ->
    get_networks(NetworkIds, []).

get_networks([], Acc) ->
    {ok, Acc};
get_networks([NetworkId | NetworkIds], Acc) ->
    case k_storage_networks:get_network(NetworkId) of
        {ok, Network} ->
            get_networks(NetworkIds, [Network | Acc]);
        Error ->
            Error
    end.

get_providers(Networks) ->
    ProvIds = lists:usort([N#network.provider_id || N <- Networks]),
    get_providers(ProvIds, []).

get_providers([], Acc) ->
    {ok, Acc};
get_providers([ProvId | ProvIds], Acc) ->
    case k_storage_providers:get_provider(ProvId) of
        {ok, Provider} ->
            get_providers(ProvIds, [Provider | Acc]);
        Error ->
            Error
    end.

network_to_dto(Network) ->
    #network{
        id = Id,
        name = Name,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderId,
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
