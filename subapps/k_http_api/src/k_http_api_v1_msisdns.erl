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
-include_lib("k_storage/include/mailbox.hrl").

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
    read_many(Msisdn, CustomerUuid, State).

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
            ok = k_storage_msisdns:assign_to(Msisdn, CustomerUuid),
            Resp = prepare(Msisdn, CustomerUuid),
            {http_code, 200, Resp}
    end.

delete(Params) ->
    Msisdn = ?gv(msisdn, Params),
    k_storage_msisdns:delete(Msisdn),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_many(Msisdn, CustomerUuid, State) ->
    {ok, Values} = k_storage_msisdns:get_many(Msisdn, CustomerUuid, State),
    Resp = prepare_values(Values),
    {http_code, 200, Resp}.

prepare(Msisdn, CustomerUuid) ->
    [{msisdn, msisdn2addr(Msisdn)},
     {customer_uuid, CustomerUuid}].

prepare_values(Values) ->
    prepare_values(Values, []).

prepare_values([], Acc) ->
    lists:reverse(Acc);
prepare_values([{Msisdn, CustomerUuid} | Values], Acc) ->
    Res = [{msisdn, msisdn2addr(Msisdn)}, {customer_uuid, CustomerUuid}],
    prepare_values(Values, [Res | Acc]).

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
