-module(k_http_api_handler_customers).

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
        #param{name = customer_uuid, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = customer_id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = false, repeated = false, type = binary},
        #param{name = priority, mandatory = false, repeated = false, type = integer},
        #param{name = rps, mandatory = false, repeated = false, type = integer},
        #param{name = network_map_id, mandatory = false, repeated = false, type = binary},
        #param{name = default_provider_id, mandatory = false, repeated = false, type = binary},
        #param{name = receipts_allowed, mandatory = false, repeated = false, type = boolean},
        #param{name = no_retry, mandatory = false, repeated = false, type = boolean},
        #param{name = default_validity, mandatory = false, repeated = false, type = binary},
        #param{name = max_validity, mandatory = false, repeated = false, type = integer},
        #param{name = pay_type, mandatory = false, repeated = false, type =
            {custom, fun pay_type/1}},
        #param{name = credit, mandatory = false, repeated = false, type = float},
        #param{name = credit_limit, mandatory = false, repeated = false, type = float},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun customer_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = false, repeated = false, type = binary},
        #param{name = customer_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = priority, mandatory = false, repeated = false, type = integer},
        #param{name = rps, mandatory = false, repeated = false, type = integer},
        #param{name = network_map_id, mandatory = true, repeated = false, type = binary},
        #param{name = default_provider_id, mandatory = true, repeated = false, type = binary},
        #param{name = receipts_allowed, mandatory = true, repeated = false, type = boolean},
        #param{name = no_retry, mandatory = true, repeated = false, type = boolean},
        #param{name = default_validity, mandatory = true, repeated = false, type = binary},
        #param{name = max_validity, mandatory = true, repeated = false, type = integer},
        #param{name = pay_type, mandatory = true, repeated = false, type =
            {custom, fun pay_type/1}},
        #param{name = credit, mandatory = true, repeated = false, type = float},
        #param{name = credit_limit, mandatory = true, repeated = false, type = float},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun customer_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/customers/[:customer_uuid]"
    }}.

create(Params) ->
    case ?gv(customer_uuid, Params) of
        undefined ->
            CustomerUuid = uuid:unparse(uuid:generate_time()),
            create_customer(lists:keyreplace(customer_uuid, 1, Params, {customer_uuid, CustomerUuid}));
        _ ->
            is_exist(Params)
    end.

is_exist(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_customer(Params);
        Error ->
            ?log_debug("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case CustomerUuid of
        undefined ->
            read_all();
        _ ->
            read_customer_uuid(CustomerUuid)
    end.

read_all() ->
    case k_storage_customers:get_customers() of
        {ok, Customers} ->
            {ok, Plists} = prepare(Customers),
            ?log_debug("Customers: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_customer_uuid(CustomerUuid) ->
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            {ok, [Plist]} = prepare(Customer),
            ?log_debug("Customer: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_customer(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_debug("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.


delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    ok = k_j3_support:delete_customer(CustomerUuid),
    ok = k_storage_customers:del_customer(CustomerUuid),
    {http_code, 204}.

%% ===================================================================
%% Internal
%% ===================================================================

update_customer(Customer, Params) ->
    CustomerUuid = Customer#customer.customer_uuid,
    NewCustomerId = ?gv(customer_id, Params, Customer#customer.customer_id),
    NewName = ?gv(name, Params, Customer#customer.name),
    NewPriority = ?gv(priority, Params, Customer#customer.priority),
    NewRps = ?gv(rps, Params, Customer#customer.rps),
    NewNetworkMapId = ?gv(network_map_id, Params, Customer#customer.network_map_id),
    NewDefaultProviderId = ?gv(default_provider_id, Params, Customer#customer.default_provider_id),
    NewReceiptsAllowed = ?gv(receipts_allowed, Params, Customer#customer.receipts_allowed),
    NewNoRetry = ?gv(no_retry, Params, Customer#customer.no_retry),
    NewDefaultValidity = ?gv(default_validity, Params, Customer#customer.default_validity),
    NewMaxValidity = ?gv(max_validity, Params, Customer#customer.max_validity),
    NewPayType = ?gv(pay_type, Params, Customer#customer.pay_type),
    NewCredit = ?gv(credit, Params, Customer#customer.credit),
    NewCreditLimit = ?gv(credit_limit, Params, Customer#customer.credit_limit),
    NewLanguage = ?gv(language, Params, Customer#customer.language),
    NewState = ?gv(state, Params, Customer#customer.state),
    NewCustomer = #customer{
        customer_uuid = CustomerUuid,
        customer_id = NewCustomerId,
        name = NewName,
        priority = NewPriority,
        rps = NewRps,
        originators = Customer#customer.originators,
        network_map_id = NewNetworkMapId,
        default_provider_id = NewDefaultProviderId,
        receipts_allowed = NewReceiptsAllowed,
        no_retry = NewNoRetry,
        default_validity = NewDefaultValidity,
        max_validity = NewMaxValidity,
        users = Customer#customer.users,
        pay_type = NewPayType,
        credit = NewCredit,
        credit_limit = NewCreditLimit,
        language = NewLanguage,
        state = NewState
    },

    ok = k_j3_support:set_customer(CustomerUuid, NewRps, NewPriority),
    ok = k_storage_customers:set_customer(CustomerUuid, NewCustomer),
    {ok, [Plist]} = prepare(NewCustomer),
    ?log_debug("Customer: ~p", [Plist]),
    {http_code, 200, Plist}.

create_customer(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    Priority = ?gv(priority, Params),
    Rps = ?gv(rps, Params),
    Customer = #customer{
        customer_uuid = CustomerUuid,
        customer_id = ?gv(customer_id, Params),
        name = ?gv(name, Params),
        priority = Priority,
        rps = Rps,
        originators = [],
        network_map_id = ?gv(network_map_id, Params),
        default_provider_id = ?gv(default_provider_id, Params),
        receipts_allowed = ?gv(receipts_allowed, Params),
        no_retry = ?gv(no_retry, Params),
        default_validity = ?gv(default_validity, Params),
        max_validity = ?gv(max_validity, Params),
        users = [],
        pay_type = ?gv(pay_type, Params),
        credit = ?gv(credit, Params),
        credit_limit = ?gv(credit_limit, Params),
        language = ?gv(language, Params),
        state = ?gv(state, Params)
    },
    ok = k_j3_support:set_customer(CustomerUuid, Rps, Priority),
    ok = k_storage_customers:set_customer(CustomerUuid, Customer),
    {ok, [Plist]} = prepare(Customer),
    ?log_debug("Customer: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare(ItemList) when is_list(ItemList) ->
    prepare(ItemList, []);
prepare(Item) ->
    prepare([Item], []).

prepare([], Acc) ->
    {ok, Acc};
prepare([Customer = #customer{} | Rest], Acc) ->
     #customer{
        originators = Originators,
        users = Users
    } = Customer,

    {ok, OriginatorPlists} =
        k_http_api_handler_customers_originators:prepare_originators(Originators),
    {ok, UserPlists} =
        k_http_api_handler_customers_users:prepare_users(Users),

    %% preparation customer's record
    Fun = ?record_to_proplist(customer),
    Plist = Fun(
        Customer#customer{
            users = UserPlists,
            originators = OriginatorPlists
        }
    ),

    ?log_debug("Customer: ~p", [Plist]),
    prepare(Rest, [Plist | Acc]).

customer_state(State) ->
    case State of
        <<"active">> -> active;
        <<"blocked">> -> blocked;
        <<"deactivated">> -> deactivated
    end.

pay_type(Type) ->
    case Type of
        <<"prepaid">> -> prepaid;
        <<"postpaid">> -> postpaid
    end.
