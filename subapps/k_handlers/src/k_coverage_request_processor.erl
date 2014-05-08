-module(k_coverage_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
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
                            case get_providers_add_points(Networks) of
                                {ok, AddPoints} ->
                                    NetworksDTO =
                                        [network_to_dto(N, AddPoints) || N <- Networks],
                                    {ok, #k1api_coverage_response_dto{
                                        id = ReqId,
                                        networks = NetworksDTO
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

get_providers_add_points(Networks) ->
    ProvIds = lists:usort([N#network.provider_id || N <- Networks]),
    get_providers_add_points(ProvIds, []).

get_providers_add_points([], Acc) ->
    {ok, Acc};
get_providers_add_points([ProvId | ProvIds], Acc) ->
    case k_storage_providers:get_provider(ProvId) of
        {ok, #provider{sms_add_points = AddPoints}} ->
            get_providers_add_points(ProvIds, [{ProvId, AddPoints} | Acc]);
        Error ->
            Error
    end.

network_to_dto(Network, ProvAddPoints) ->
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
    AddPoints = proplists:get_value(ProviderId, ProvAddPoints),
    SmsCost = (SmsPoints + AddPoints) * SmsMultPoints,
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
        sms_cost = SmsCost
    }.
