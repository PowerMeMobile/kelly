-module(k_amqp_api_handler).

-export([start_link/0]).
-export([process/2]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/network.hrl").

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

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"CoverageReq">>, <<>>) ->
    ?log_debug("Got coverage request", []),
    {ok, Networks} = k_config:get_networks(),
    {ok, AddCredits} = get_providers_add_credits(Networks),
    CoverageRespDTO = #k1api_coverage_response_dto{
        networks = [network_to_dto(Id, N, AddCredits) || {Id, N} <- Networks]
    },
    ?log_debug("Built coverage response: ~p", [CoverageRespDTO]),
    case adto:encode(CoverageRespDTO) of
        {ok, Bin} ->
            {<<"CoverageResp">>, Bin};
        {error, Error} ->
            ?log_error("Coverage response decode error: ~p", [Error]),
            {ok, []}
    end.

get_providers_add_credits(Networks) ->
    ProvIds = lists:usort([N#network.provider_id || {_, N} <- Networks]),
    get_providers_add_credits(ProvIds, []).

get_providers_add_credits([], Acc) ->
    {ok, Acc};
get_providers_add_credits([ProvId | ProvIds], Acc) ->
    case k_config:get_provider(ProvId) of
        {ok, #provider{sms_add_credits = AddCredits}} ->
            get_providers_add_credits(ProvIds, [{ProvId, AddCredits} | Acc]);
        Error ->
            Error
    end.

network_to_dto(Id, Network, ProvAddCredits) ->
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
        sms_mult_credits = SmsMultCredits
    } = Network,
    AddCredits = proplists:get_value(ProviderId, ProvAddCredits),
    SmsCost = (SmsPoints + AddCredits) * SmsMultCredits,
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
