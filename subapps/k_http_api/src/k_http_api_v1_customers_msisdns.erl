-module(k_http_api_v1_customers_msisdns).

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
    Read = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = msisdn, mandatory = true, repeated = false, type =
            {custom, fun decode_msisdn/1}}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = msisdn, mandatory = true, repeated = false, type =
            {custom, fun decode_msisdn/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        delete = Delete,
        route = "/v1/customers/:customer_uuid/msisdns/[:msisdn]"
    }}.

create(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    Msisdn = ?gv(msisdn, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{}} ->
            case k_storage_msisdns:get_one(Msisdn) of
                {error, not_found} ->
                    {exception, 'svc0003'};
                {ok, {Msisdn, undefined}} ->
                    ok = k_storage_msisdns:assign_to(Msisdn, CustomerUuid),
                    {http_code, 201, <<"">>};
                {ok, {Msisdn, CustomerUuid}} ->
                    {exception, 'svc0004'};
                {ok, {Msisdn, _}} ->
                    {http_code, 409, <<"Msisdn already assigned">>}
            end;
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{}} ->
            {ok, Msisdns} = k_storage_msisdns:get_assigned_to(CustomerUuid),
            {ok, Plist} = prepare_msisdns(Msisdns),
            ?log_debug("Msisdns: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update(_Params) ->
    ok.

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    Msisdn = ?gv(msisdn, Params),
    case k_storage_msisdns:unassign_from(Msisdn, CustomerUuid) of
        ok ->
            {http_code, 204};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec prepare_msisdns(#addr{} | [#addr{}]) -> {ok, [{atom(), term()}]}.
prepare_msisdns(Msisdn = #addr{}) ->
    AddrFun = ?record_to_proplist(addr),
    proplists:delete(ref_num, AddrFun(Msisdn));
prepare_msisdns(Msisdns) when is_list(Msisdns) ->
    {ok, [prepare_msisdns(M) || M <- Msisdns]}.

%% convert "addr,ton,npi" to #addr{addr, ton, npi}
decode_msisdn(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.
