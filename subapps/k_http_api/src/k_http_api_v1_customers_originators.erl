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
            State = ?gv(state, Params),
            Originator = #originator{
                id = Id,
                address = Msisdn,
                description = Description,
                is_default = IsDefault,
                state = State
            },
            ok = k_storage_customers:set_originator(Originator, Customer#customer.customer_uuid),
            {ok, [Plist]} = k_http_api_utils:prepare_originators([Originator]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 201, Plist}
    end.

update_originator(Customer, Params) ->
    Id = ?gv(id, Params),
    case k_storage_customers:get_originator(Customer, Id) of
        {ok, Originator} ->
            Msisdn = ?gv(msisdn, Params, Originator#originator.address),
            Description = ?gv(description, Params, Originator#originator.description),
            IsDefault = ?gv(is_default, Params, Originator#originator.is_default),
            State = ?gv(state, Params, Originator#originator.state),
            Updated = #originator{
                id = Id,
                address = Msisdn,
                description = Description,
                is_default = IsDefault,
                state = State
            },
            ok = k_storage_customers:set_originator(Updated, Customer#customer.customer_uuid),
            {ok, [Plist]} = k_http_api_utils:prepare_originators([Updated]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

get_originator(Customer, undefined) ->
    #customer{originators = Originators} = Customer,
    {ok, Plist} = k_http_api_utils:prepare_originators(Originators),
    ?log_debug("Originator: ~p", [Plist]),
    {http_code, 200, Plist};
get_originator(Customer, Id) ->
    case k_storage_customers:get_originator(Customer, Id) of
        {ok, Originator} ->
            {ok, [Plist]} = k_http_api_utils:prepare_originators([Originator]),
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
