-module(k_http_api_v1_msisdns).

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
-include_lib("k_storage/include/msisdn.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Create = [
        #param{name = msisdn, mandatory = true, repeated = false, type = {custom, fun decode_msisdn/1}}
    ],
    Read = [
        #param{name = msisdn, mandatory = false, repeated = false, type = {custom, fun decode_msisdn/1}},
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = state, mandatory = false, repeated = false, type = {custom, fun decode_state/1}}
    ],
    Update = [
        #param{name = msisdn, mandatory = true, repeated = false, type = {custom, fun decode_msisdn/1}},
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid}
    ],
    Delete = [
        #param{name = msisdn, mandatory = true, repeated = false, type = {custom, fun decode_msisdn/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/msisdns/[:msisdn]"
    }}.

read(Params) ->
    Msisdn = ?gv(msisdn, Params),
    CustomerUuid = ?gv(customer_uuid, Params),
    State = ?gv(state, Params, all),
    {ok, Infos} = k_storage_msisdns:get_many(Msisdn, CustomerUuid, State),
    Uuids = [Uuid || I <- Infos,
             begin
                Uuid = I#msisdn_info.customer_uuid,
                Uuid =/= undefined
             end],
    Dict = k_storage_utils:get_uuid_to_customer_dict(Uuids),
    Resp = [build_resp(I, Dict) || I <- Infos],
    {http_code, 200, Resp}.

create(Params) ->
    Msisdn = ?gv(msisdn, Params),
    case k_storage_msisdns:get_one(Msisdn) of
        {error, not_found} ->
            ok = k_storage_msisdns:create(Msisdn),
            {http_code, 201, <<"">>};
        {ok, _} ->
            {exception, 'svc0004'}
    end.

update(Params) ->
    Msisdn = ?gv(msisdn, Params),
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_msisdns:get_one(Msisdn) of
        {error, not_found} ->
            {exception, 'svc0003'};
        {ok, _} ->
            ok = k_storage_msisdns:assign_to_customer(Msisdn, CustomerUuid),
            {http_code, 200, <<"">>}
    end.

delete(Params) ->
    Msisdn = ?gv(msisdn, Params),
    k_storage_msisdns:delete(Msisdn),
    {http_code, 204}.

%% ===================================================================
%% Internal
%% ===================================================================

build_resp(Info, Dict) ->
    Msisdn = Info#msisdn_info.msisdn,
    CustomerUuid = Info#msisdn_info.customer_uuid,
    {CustomerId, CustomerName} =
        case CustomerUuid of
            undefined ->
                {undefined, undefined};
            _ ->
                Customer = dict:fetch(CustomerUuid, Dict),
                {Customer#customer.customer_id, Customer#customer.name}
        end,
    [{msisdn, msisdn2addr(Msisdn)},
     {customer_uuid, CustomerUuid},
     {customer_id, CustomerId},
     {customer_name, CustomerName}].

decode_msisdn(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.

decode_state(<<"all">>)  -> all;
decode_state(<<"free">>) -> free;
decode_state(<<"used">>)  -> used.

msisdn2addr(Msisdn) ->
    [{addr, Msisdn#addr.addr},
     {ton, Msisdn#addr.ton},
     {npi, Msisdn#addr.npi}].
