-module(k_http_api_v1_customers).

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

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = customer_id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = false, repeated = false, type = binary},
        #param{name = priority, mandatory = false, repeated = false, type = integer},
        #param{name = rps, mandatory = false, repeated = false, type = integer},
        #param{name = network_map_id, mandatory = false, repeated = false, type = uuid},
        %% the type should be uuid, but it's not clear how to allow empty uuid
        #param{name = default_provider_id, mandatory = false, repeated = false, type = binary},
        #param{name = receipts_allowed, mandatory = false, repeated = false, type = boolean},
        #param{name = no_retry, mandatory = false, repeated = false, type = boolean},
        #param{name = default_validity, mandatory = false, repeated = false, type = binary},
        #param{name = max_validity, mandatory = false, repeated = false, type = integer},
        #param{name = interfaces, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = pay_type, mandatory = false, repeated = false, type =
            {custom, fun decode_pay_type/1}},
        #param{name = credit, mandatory = false, repeated = false, type = float},
        #param{name = credit_limit, mandatory = false, repeated = false, type = float},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = customer_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = priority, mandatory = false, repeated = false, type = integer},
        #param{name = rps, mandatory = false, repeated = false, type = integer},
        #param{name = network_map_id, mandatory = true, repeated = false, type = uuid},
        %% the type should be uuid, but it's not clear how to allow empty uuid
        #param{name = default_provider_id, mandatory = true, repeated = false, type = binary},
        #param{name = receipts_allowed, mandatory = true, repeated = false, type = boolean},
        #param{name = no_retry, mandatory = true, repeated = false, type = boolean},
        #param{name = default_validity, mandatory = true, repeated = false, type = binary},
        #param{name = max_validity, mandatory = true, repeated = false, type = integer},
        #param{name = interfaces, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = pay_type, mandatory = true, repeated = false, type =
            {custom, fun decode_pay_type/1}},
        #param{name = credit, mandatory = true, repeated = false, type = float},
        #param{name = credit_limit, mandatory = true, repeated = false, type = float},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/customers/[:customer_uuid]"
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
            create_customer(Params)
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
            {ok, Plists} = prepare_customers(Customers),
            ?log_debug("Customers: ~p", [Plists]),
            {http_code, 200, Plists}
    end.

read_customer_uuid(CustomerUuid) ->
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            {ok, [Plist]} = prepare_customers(Customer),
            ?log_debug("Customer: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_customer(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    ok = k_control_just:delete_customer(CustomerUuid),
    ok = k_storage_msisdns:unassign_all_from_customer(CustomerUuid),
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

    Users = Customer#customer.users,

    PreInterfaces = Customer#customer.interfaces,
    NewInterfaces = ?gv(interfaces, Params, PreInterfaces),
    Users2 = sync_interfaces(PreInterfaces, NewInterfaces, Users),

    PreFeatures = Customer#customer.features,
    NewFeatures = ?gv(features, Params, PreFeatures),
    Users3 = sync_features(PreFeatures, NewFeatures, Users2),

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
        users = Users3,
        interfaces = NewInterfaces,
        features = NewFeatures,
        pay_type = NewPayType,
        credit = NewCredit,
        credit_limit = NewCreditLimit,
        language = NewLanguage,
        state = NewState
    },

    ok = k_control_just:set_customer(CustomerUuid, NewRps, NewPriority),
    ok = k_storage_customers:set_customer(CustomerUuid, NewCustomer),
    {ok, [Plist]} = prepare_customers(NewCustomer),
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
        interfaces = ?gv(interfaces, Params),
        features = ?gv(features, Params),
        pay_type = ?gv(pay_type, Params),
        credit = ?gv(credit, Params),
        credit_limit = ?gv(credit_limit, Params),
        language = ?gv(language, Params),
        state = ?gv(state, Params)
    },
    ok = k_control_just:set_customer(CustomerUuid, Rps, Priority),
    ok = k_storage_customers:set_customer(CustomerUuid, Customer),
    {ok, [Plist]} = prepare_customers(Customer),
    ?log_debug("Customer: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare_customers(ItemList) when is_list(ItemList) ->
    prepare_customers(ItemList, []);
prepare_customers(Item) ->
    prepare_customers([Item], []).

prepare_customers([], Acc) ->
    {ok, Acc};
prepare_customers([Customer = #customer{} | Rest], Acc) ->
     #customer{
        originators = Originators,
        users = Users,
        features = Features
    } = Customer,

    {ok, OriginatorPlists} =
        k_http_api_utils:prepare_originators(Customer, Originators),
    {ok, UserPlists} =
        k_http_api_utils:prepare_users(Customer, Users),
    {ok, FeaturesPlists} =
        k_http_api_utils:prepare_features(Features),

    %% preparation customer's record
    Fun = ?record_to_proplist(customer),
    Plist = Fun(
        Customer#customer{
            users = UserPlists,
            originators = OriginatorPlists,
            features = FeaturesPlists
        }
    ),
    prepare_customers(Rest, [Plist | Acc]).

sync_interfaces(PreIfs, NewIfs, Users) ->
    DisIfs = get_disabled_interfaces(PreIfs, NewIfs),
    Users2 = [U#user{
                interfaces = U#user.interfaces -- DisIfs
              } || U <- Users],
    case lists:member(email, DisIfs) of
        true ->
            [U#user{
                features = remove_features(
                    U#user.features, [<<"sms_from_email">>])
             } || U <- Users2];
        false ->
            Users2
    end.

sync_features(PreFs, NewFs, Users) ->
    DisNames = get_disabled_feature_names(PreFs, NewFs),
    [U#user{features = remove_features(U#user.features, DisNames)} ||
     U <- Users].

get_disabled_interfaces(PreIfs, NewIfs) ->
    PreIfs -- NewIfs.

get_disabled_feature_names(PreFs, NewFs) ->
    Fs = lists:filter(
        fun(Feature=#feature{name=Name}) ->
            case lists:keyfind(Name, #feature.name, NewFs) of
                Feature ->
                    %% feature exists
                    %% filter it out
                    false;
                _Otherwise ->
                    %% feature was either removed or disabled
                    %% leave it
                    true
            end
        end,
        PreFs
    ),
    [N || #feature{name = N} <- Fs].

remove_features(Features, DisNames) ->
    [F || F <- Features, not lists:member(F#feature.name, DisNames)].

decode_state(State) ->
    case bstr:lower(State) of
        <<"active">>      -> active;
        <<"blocked">>     -> blocked;
        <<"deactivated">> -> deactivated
    end.

decode_pay_type(PayType) ->
    case bstr:lower(PayType) of
        <<"prepaid">>  -> prepaid;
        <<"postpaid">> -> postpaid
    end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

sync_interfaces_test() ->
    PreIfs = [soap,mm,email],
    NewIfs = [soap],
    PreUsers = [
        #user{interfaces = [soap,email]}
    ],
    NewUsers = [
        #user{interfaces = [soap]}
    ],
    ?assertEqual(NewUsers, sync_interfaces(PreIfs, NewIfs, PreUsers)).

disable_email_interface_removes_sms_from_email_feature_test() ->
    PreIfs = [email],
    NewIfs = [],
    PreUsers = [
        #user{
            interfaces = [email],
            features = [#feature{name = <<"sms_from_email">>, value = <<"true">>}]
        }
    ],
    NewUsers = [
        #user{interfaces = [], features = []}
    ],
    ?assertEqual(NewUsers, sync_interfaces(PreIfs, NewIfs, PreUsers)).

get_disabled_feature_names_test() ->
    PreFs = [
        #feature{name = <<"f1">>, value = <<"true">>},
        #feature{name = <<"f2">>, value = <<"true">>},
        #feature{name = <<"f3">>, value = <<"true">>},
        #feature{name = <<"f4">>, value = <<"false">>}
    ],
    NowFs = [
        #feature{name = <<"f1">>, value = <<"false">>},
        #feature{name = <<"f3">>, value = <<"true">>}
    ],
    DisabledFs = [
        <<"f1">>,
        <<"f2">>,
        <<"f4">>
    ],
    ?assertEqual(DisabledFs, get_disabled_feature_names(PreFs, NowFs)).

remove_features_test() ->
    Fs = [
        #feature{name = <<"f1">>, value = <<"true">>},
        #feature{name = <<"f2">>, value = <<"true">>},
        #feature{name = <<"f3">>, value = <<"true">>},
        #feature{name = <<"f4">>, value = <<"false">>}
    ],
    DisNames = [
        <<"f1">>,
        <<"f2">>,
        <<"f4">>
    ],
    LeftFs = [
        #feature{name = <<"f3">>, value = <<"true">>}
    ],
    ?assertEqual(LeftFs, remove_features(Fs, DisNames)).

sync_features_test() ->
    PreFs = [#feature{name = <<"inbox">>, value = <<"true">>}],
    NowFs = [],
    PreUsers = [
        #user{features = [#feature{name = <<"inbox">>, value = <<"true">>}]}
    ],
    NewUsers = [
        #user{features = []}
    ],
    ?assertEqual(NewUsers, sync_features(PreFs, NowFs, PreUsers)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
