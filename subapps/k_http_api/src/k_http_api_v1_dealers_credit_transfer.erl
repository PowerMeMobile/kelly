-module(k_http_api_v1_dealers_credit_transfer).

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

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Create = [
        #param{name = dealer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = from_customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = to_customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = amount, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = [],
        update = [],
        delete = [],
        route = "/v1/dealers/[:dealer_uuid]/credit_transfer"
    }}.


create(Params) ->
    DealerUUID = ?gv(dealer_uuid, Params),
    FromCustomerUUID = ?gv(from_customer_uuid, Params),
    ToCustomerUUID = ?gv(to_customer_uuid, Params),
    Amount = ?gv(amount, Params),
    {ok, TransactionUUID} =
    k_storage_customers:transfer_credit(DealerUUID,
                                        FromCustomerUUID,
                                        ToCustomerUUID,
                                        Amount),
    Resp = [
        {transaction_id, TransactionUUID}
    ],
    {http_code, 200, Resp}.


read(_Params) ->
    {http_code, 400}.


update(_Params) ->
    {http_code, 400}.


delete(_Params) ->
    {http_code, 400}.

%% ===================================================================
%% Internal
%% ===================================================================
