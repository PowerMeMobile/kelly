-module(k_http_api_v1_customers_originators).

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
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = id, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = msisdn, mandatory = false, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}},
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = is_default, mandatory = false, repeated = false, type = boolean},
        #param{name = routings, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_routing/1}},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = msisdn, mandatory = true, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}},
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = is_default, mandatory = false, repeated = false, type = boolean},
        #param{name = routings, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_routing/1}},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/customers/:customer_uuid/originators/[:id]"
    }}.

create(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            create_originator(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            get_originator(Customer, ?gv(id, Params));
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_originator(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    OriginatorId = ?gv(id, Params),
    case k_storage_customers:del_originator_by_id(CustomerUuid, OriginatorId) of
        {error, no_entry} ->
            ?log_warn("Customer [~p] not found", [CustomerUuid]),
            {exception, 'svc0003'};
        ok ->
            {http_code, 204}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

create_originator(Customer, Params) ->
    Id = case ?gv(id, Params) of
        undefined ->
            uuid:unparse(uuid:generate_time());
        GivenId ->
            GivenId
    end,
    case k_storage_customers:get_originator(Customer, Id) of
        {ok, #originator{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            Msisdn = ?gv(msisdn, Params),
            Description = ?gv(description, Params),
            IsDefault = ?gv(is_default, Params),
            Routings = ?gv(routings, Params),
            State = ?gv(state, Params),
            O = #originator{
                id = Id,
                address = Msisdn,
                description = Description,
                is_default = IsDefault,
                routings = Routings,
                state = State
            },
            ok = k_storage_customers:set_originator(O, Customer#customer.customer_uuid),
            {ok, [Plist]} = k_http_api_utils:prepare_originators(Customer, [O]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 201, Plist}
    end.

update_originator(Customer, Params) ->
    Id = ?gv(id, Params),
    case k_storage_customers:get_originator(Customer, Id) of
        {ok, O} ->
            Msisdn = ?gv(msisdn, Params, O#originator.address),
            Description = ?gv(description, Params, O#originator.description),
            IsDefault = ?gv(is_default, Params, O#originator.is_default),
            Routings = ?gv(routings, Params, O#originator.routings),
            State = ?gv(state, Params, O#originator.state),
            O2 = #originator{
                id = Id,
                address = Msisdn,
                description = Description,
                is_default = IsDefault,
                routings = Routings,
                state = State
            },
            ok = k_storage_customers:set_originator(O2, Customer#customer.customer_uuid),
            {ok, [Plist]} = k_http_api_utils:prepare_originators(Customer, [O2]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

get_originator(Customer, undefined) ->
    #customer{originators = Os} = Customer,
    {ok, Plist} = k_http_api_utils:prepare_originators(Customer, Os),
    ?log_debug("Originator: ~p", [Plist]),
    {http_code, 200, Plist};
get_originator(Customer, Id) ->
    case k_storage_customers:get_originator(Customer, Id) of
        {ok, O} ->
            {ok, [Plist]} = k_http_api_utils:prepare_originators(Customer, [O]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

decode_state(State) ->
    case bstr:lower(State) of
        <<"pending">> -> pending;
        <<"approved">> -> approved;
        <<"rejected">> -> rejected
    end.
