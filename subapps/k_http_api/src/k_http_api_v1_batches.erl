-module(k_http_api_v1_batches).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = from, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = to, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = user_id, mandatory = false, repeated = false, type = binary},
        #param{name = skip, mandatory = true, repeated = false, type = integer},
        #param{name = limit, mandatory = true, repeated = false, type = integer}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/batches"
    }}.

read(Params) ->
    {ok, Report} =
        case ?gv(customer_uuid, Params) of
            undefined ->
                k_statistic_mt_batches:get_all(Params);
            CustomerUuid ->
                %% don't do the report until the customer exists
                case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
                    {ok, #customer{}} ->
                        k_statistic_mt_batches:get_all(Params);
                    {error, no_entry} ->
                        {ok, []}
                end
        end,
    {ok, Report}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
