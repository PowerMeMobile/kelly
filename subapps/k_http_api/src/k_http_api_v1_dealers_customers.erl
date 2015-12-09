-module(k_http_api_v1_dealers_customers).

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
        #param{name = id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/dealers/:id/customers"
    }}.


create(_Params) ->
    {http_code, 400}.


read(Params) ->
    DealerUUID = ?gv(id, Params),
    case k_storage_customers:get_customers_by_dealer_uuid(DealerUUID) of
        {ok, Customers} ->
            {ok, Plists} = k_http_api_v1_customers:prepare_customers(Customers),
            ?log_debug("Dealer customers: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Reason} ->
            ?log_error("Cant get dealer customers: ~p", [Reason]),
            {http_code, 500}
    end.


update(_Params) ->
    {http_code, 400}.


delete(_Params) ->
    {http_code, 400}.

%% ===================================================================
%% Internal
%% ===================================================================
