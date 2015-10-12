-module(k_coverage_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #coverage_req_v1{}) ->
    ReqId      = Req#coverage_req_v1.req_id,
    CustomerId = Req#coverage_req_v1.customer_id,

    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            NetMapId = Customer#customer.network_map_id,
            DefProvId = Customer#customer.default_provider_id,
            {ok, Networks, Providers} =
                k_handlers_utils:get_networks_and_providers(NetMapId, DefProvId),
             NetworksV1 = [k_handlers_utils:network_to_v1(N) || N <- Networks],
             ProvidersV1 = [k_handlers_utils:provider_to_v1(P) || P <- Providers],
             {ok, #coverage_resp_v1{
                req_id = ReqId,
                networks = NetworksV1,
                providers = ProvidersV1,
                default_provider_id = DefProvId
              }};
        {error, no_entry} ->
            ?log_error("Customer id: ~p not found", [CustomerId]),
            {error, unknown_customer}
    end.
