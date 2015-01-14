-module(k_http_api_handler_customers_credit).

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
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = amount, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        update = Update,
        route = "/customers/:customer_uuid/credit"
    }}.

read(_Params) ->
    ok.

create(_Params) ->
    ok.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    Amount = ?gv(amount, Params),

    case k_storage_customers:change_credit(CustomerUuid, Amount) of
        {ok, _NewCredit} ->
            {http_code, 200};
        {error, no_entry} ->
            ?log_error("Customer not found (customer_uuid: ~p)", [CustomerUuid]),
            {exception, 'svc0003'};
        {error, Error} ->
            ?log_error("Change credit (customer_uuid: ~p, amount: ~p) failed with: ~p",
                [CustomerUuid, Amount, Error]),
            {http_code, 500}
    end.

delete(_Params) ->
    ok.
