-module(k_amqp_api_handler).

-export([start_link/0]).
-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/network.hrl").
-include_lib("k_storage/include/network_map.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Queue} = application:get_env(k_handlers, kelly_api_queue),
    rmql_rpc_server:start_link(?MODULE, Queue, fun ?MODULE:process/2).

%% ===================================================================
%% Internal
%% ===================================================================

-spec process(binary(), binary()) -> {binary(), binary()} | {ok, []}.
process(<<"CoverageReq">>, ReqBin) ->
    case adto:decode(#k1api_coverage_request_dto{}, ReqBin) of
        {ok, CoverageReqDTO} ->
            ?log_debug("Got coverage request: ~p", [CoverageReqDTO]),
            case process_coverage_request(CoverageReqDTO) of
                {ok, CoverageRespDTO} ->
                    ?log_debug("Built coverage response: ~p", [CoverageRespDTO]),
                    case adto:encode(CoverageRespDTO) of
                        {ok, RespBin} ->
                            {<<"CoverageResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Coverage response decode error: ~p", [Error]),
                            {ok, []}
                    end;
                {error, Error} ->
                    ?log_error("Coverage request process error: ~p", [Error]),
                    {ok, []}
            end;
        {error, Error} ->
            ?log_error("Coverage requeste decode error: ~p", [Error]),
            {ok, []}
    end.

process_coverage_request(CoverageReqDTO) ->
    ReqId      = CoverageReqDTO#k1api_coverage_request_dto.id,
    CustomerId = CoverageReqDTO#k1api_coverage_request_dto.customer_id,
    _UserId    = CoverageReqDTO#k1api_coverage_request_dto.user_id,
    _Version   = CoverageReqDTO#k1api_coverage_request_dto.version,
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
                                        [network_to_dto(Id, N, AddPoints) || {Id, N} <- Networks],
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

get_networks(NetworkIds) ->
    get_networks(NetworkIds, []).

get_networks([], Acc) ->
    {ok, Acc};
get_networks([NetworkId | NetworkIds], Acc) ->
    case k_storage_networks:get_network(NetworkId) of
        {ok, Network} ->
            get_networks(NetworkIds, [{NetworkId, Network} | Acc]);
        Error ->
            Error
    end.

get_providers_add_points(Networks) ->
    ProvIds = lists:usort([N#network.provider_id || {_, N} <- Networks]),
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

network_to_dto(Id, Network, ProvAddPoints) ->
    #network{
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
