-module(k_http_api_v1_customers_users).

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
        #param{name = user_id, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = false, repeated = false, type = binary},
        #param{name = interfaces, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = false, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = mobile_phone, mandatory = false, repeated = false, type = binary},
        #param{name = first_name, mandatory = false, repeated = false, type = binary},
        #param{name = last_name, mandatory = false, repeated = false, type = binary},
        #param{name = company, mandatory = false, repeated = false, type = binary},
        #param{name = occupation, mandatory = false, repeated = false, type = binary},
        #param{name = email, mandatory = false, repeated = false, type = binary},
        #param{name = country, mandatory = false, repeated = false, type = binary},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = user_id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = true, repeated = false, type = binary},
        #param{name = interfaces, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_interface/1}},
        #param{name = features, mandatory = true, repeated = true, type =
            {custom, fun k_http_api_utils:decode_feature/1}},
        #param{name = mobile_phone, mandatory = false, repeated = false, type = binary},
        #param{name = first_name, mandatory = false, repeated = false, type = binary},
        #param{name = last_name, mandatory = false, repeated = false, type = binary},
        #param{name = company, mandatory = false, repeated = false, type = binary},
        #param{name = occupation, mandatory = false, repeated = false, type = binary},
        #param{name = email, mandatory = false, repeated = false, type = binary},
        #param{name = country, mandatory = false, repeated = false, type = binary},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun decode_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/customers/:customer_uuid/users/[:user_id]"
    }}.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            get_user_by_id(Customer, ?gv(user_id, Params));
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

create(Params) ->
    Steps = [
        fun get_customer/1,
        fun check_user_doesnt_exist/1,
        fun check_unique_email/1,
        fun check_unique_phone/1,
        fun check_interfaces/1,
        fun check_features/1,
        fun create_user/1
    ],
    handle(Steps, Params).

update(Params) ->
    Steps = [
        fun get_customer/1,
        fun check_user_exists/1,
        fun check_unique_email/1,
        fun check_unique_phone/1,
        fun check_interfaces/1,
        fun check_features/1,
        fun update_user/1
    ],
    handle(Steps, Params).

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    ok = k_storage_msisdns:unassign_all_from_user(CustomerUuid, UserId),
    case k_storage_customers:del_user(CustomerUuid, UserId) of
        {error, no_entry} ->
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'};
        ok ->
            {http_code, 204}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

handle([Step], Params) ->
    Step(Params);
handle([Step|Steps], Params) ->
    case Step(Params) of
        {ok, Params2} ->
            handle(Steps, Params2);
        Error ->
            Error
    end.

get_customer(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            {ok, [{customer, Customer} | Params]};
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

check_user_doesnt_exist(Params) ->
    Customer = ?gv(customer, Params),
    UserId = ?gv(user_id, Params),
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, #user{}} ->
            ?log_error("User already exists: ~p", [UserId]),
            {exception, 'svc0004'};
        {error, no_entry} ->
            {ok, Params}
    end.

check_user_exists(Params) ->
    Customer = ?gv(customer, Params),
    UserId = ?gv(user_id, Params),
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, User = #user{}} ->
            {ok, [{user, User} | Params]};
        {error, no_entry} ->
            CustomerUuid = ?gv(customer_uuid, Params),
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'}
    end.

check_unique_email(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    case bstr:strip(?gv(email, Params, <<>>)) of
        <<>> ->
            {ok, Params};
        Email ->
            case k_storage_customers:get_customer_by_email(Email) of
                %% email is NOT used
                {error, no_entry} ->
                    {ok, Params};
                %% it's our customer and our user
                {ok, Customer = #customer{customer_uuid = CustomerUuid}} ->
                    case k_storage_customers:get_user_by_email(Customer, Email) of
                        {ok, #user{id = UserId}} ->
                            {ok, Params};
                        {ok, _OtherUser} ->
                            {exception, 'svc0002', [<<"email">>, Email]}
                    end;
                {ok, _OtherCustomer} ->
                    {exception, 'svc0002', [<<"email">>, Email]}
            end
    end.

check_unique_phone(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    case bstr:strip(?gv(mobile_phone, Params, <<>>)) of
        <<>> ->
            {ok, Params};
        Phone ->
            Msisdn = alley_services_utils:addr_to_dto(Phone),
            case k_storage_customers:get_customer_by_msisdn(Msisdn) of
                %% phone is NOT used
                {error, no_entry} ->
                    {ok, Params};
                %% it's our customer and our user
                {ok, Customer = #customer{customer_uuid = CustomerUuid}} ->
                    case k_storage_customers:get_user_by_msisdn(Customer, Msisdn) of
                        {ok, #user{id = UserId}} ->
                            {ok, Params};
                        {ok, _OtherUser} ->
                            {exception, 'svc0002', [<<"mobile_phone">>, Phone]}
                    end;
                {ok, _OtherCustomer} ->
                    {exception, 'svc0002', [<<"mobile_phone">>, Phone]}
            end
    end.

check_interfaces(Params) ->
    Customer = ?gv(customer, Params),
    CustomerIfs = Customer#customer.interfaces,
    Ifs = ?gv(interfaces, Params, []),
    case Ifs -- CustomerIfs of
        [] ->
            {ok, Params};
        Forbidden ->
            {exception, 'svc0002', [<<"interfaces">>, Forbidden]}
    end.

check_features(Params) ->
    Customer = ?gv(customer, Params),
    CustomerIfs = Customer#customer.interfaces,
    CustomerFs = Customer#customer.features,
    Fs = ?gv(features, Params, []),
    Forbidden = [],
    Forbidden2 =
        %% forbid sms_from_email w/o customer's email interface
        case lists:keyfind(<<"sms_from_email">>, #feature.name, Fs) of
            false ->
                Forbidden;
            #feature{value = <<"false">>} ->
                Forbidden;
            #feature{value = <<"true">>} ->
                case lists:member(email, CustomerIfs) of
                    true ->
                        Forbidden;
                    false ->
                        [<<"sms_from_email">> | Forbidden]
                end
        end,
    Forbidden3 =
        %% forbid inbox if w/o customer's inbox
        case lists:keyfind(<<"inbox">>, #feature.name, Fs) of
            false ->
                Forbidden2;
            #feature{value = <<"false">>} ->
                Forbidden2;
            #feature{value = <<"true">>} ->
                case lists:keyfind(<<"inbox">>, #feature.name, CustomerFs) of
                    false ->
                        [<<"inbox">> | Forbidden2];
                    #feature{value = <<"false">>} ->
                        [<<"inbox">> | Forbidden2];
                    #feature{value = <<"true">>} ->
                        Forbidden2
                end
        end,
    case Forbidden3 of
        [] ->
            {ok, Params};
        _ ->
            {exception, 'svc0002', [<<"features">>, Forbidden3]}
    end.

create_user(Params) ->
    UserId = ?gv(user_id, Params),
    Interfaces = ?gv(interfaces, Params),
    Features = ?gv(features, Params),
    Password = ?gv(password, Params),
    MobilePhone = ?gv(mobile_phone, Params),
    FirstName = ?gv(first_name, Params),
    LastName = ?gv(last_name, Params),
    Company = ?gv(company, Params),
    Occupation = ?gv(occupation, Params),
    Email = ?gv(email, Params),
    Country = ?gv(country, Params),
    Language = ?gv(language, Params),
    State = ?gv(state, Params),
    User = #user{
        id = UserId,
        password = ac_hexdump:binary_to_hexdump(
            crypto:hash(md5, Password), to_lower),
        interfaces = Interfaces,
        features = Features,
        mobile_phone = MobilePhone,
        first_name = FirstName,
        last_name = LastName,
        company = Company,
        occupation = Occupation,
        email = Email,
        country = Country,
        language = Language,
        state = State
    },
    Customer = ?gv(customer, Params),
    ok = k_storage_customers:set_user(User, Customer#customer.customer_uuid),
    {ok, [Plist]} = k_http_api_utils:prepare_users(Customer, [User]),
    ?log_debug("User: ~p", [Plist]),
    {http_code, 201, Plist}.

update_user(Params) ->
    User = ?gv(user, Params),
    UserId = User#user.id,
    Password = resolve_pass(?gv(password, Params), User#user.password),
    MobilePhone = ?gv(mobile_phone, Params, User#user.mobile_phone),
    FirstName = ?gv(first_name, Params, User#user.first_name),
    LastName = ?gv(last_name, Params, User#user.last_name),
    Company = ?gv(company, Params, User#user.company),
    Occupation = ?gv(occupation, Params, User#user.occupation),
    Email = ?gv(email, Params, User#user.email),
    Country = ?gv(country, Params, User#user.country),
    Language = ?gv(language, Params, User#user.language),
    State = ?gv(state, Params, User#user.state),

    PreIfs = User#user.interfaces,
    NewIfs = ?gv(interfaces, Params, PreIfs),
    DisIfs = k_http_api_utils:get_disabled_interfaces(PreIfs, NewIfs),

    PreFs = User#user.features,
    NewFs = ?gv(features, Params, PreFs),
    DisFNs = k_http_api_utils:get_disabled_feature_names(PreFs, NewFs),

    NewFs2 =
        case lists:member(email, DisIfs) of
            true ->
                k_http_api_utils:remove_features(NewFs, [<<"sms_from_email">>]);
            false ->
                NewFs
        end,

    case lists:member(<<"inbox">>, DisFNs) of
        true ->
            CustomerUuid = ?gv(customer_uuid, Params),
            ok = k_storage_msisdns:unassign_all_from_user(CustomerUuid, UserId);
        false ->
            nop
    end,

    Updated = #user{
        id = UserId,
        password = Password,
        interfaces = NewIfs,
        features = NewFs2,
        mobile_phone = MobilePhone,
        first_name = FirstName,
        last_name = LastName,
        company = Company,
        occupation = Occupation,
        email = Email,
        country = Country,
        language = Language,
        state = State
    },
    Customer = ?gv(customer, Params),
    ok = k_storage_customers:set_user(Updated, Customer#customer.customer_uuid),
    {ok, [Plist]} = k_http_api_utils:prepare_users(Customer, [Updated]),
    ?log_debug("User: ~p", [Plist]),
    {ok, Plist}.

get_user_by_id(Customer, undefined) ->
    #customer{users = Users} = Customer,
    {ok, Plist} = k_http_api_utils:prepare_users(Customer, Users),
    ?log_debug("User: ~p", [Plist]),
    {ok, Plist};
get_user_by_id(Customer, UserId) ->
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, User} ->
            {ok, [Plist]} = k_http_api_utils:prepare_users(Customer, [User]),
            ?log_debug("User: ~p", [Plist]),
            {ok, Plist};
        {error, no_entry} ->
            CustomerUuid = Customer#customer.customer_uuid,
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'}
    end.

resolve_pass(undefined, Pass) ->
    Pass;
resolve_pass(NewPass, _Pass) ->
    ac_hexdump:binary_to_hexdump(crypto:hash(md5, NewPass), to_lower).

decode_state(State) ->
    case bstr:lower(State) of
        <<"active">>      -> active;
        <<"blocked">>     -> blocked;
        <<"deactivated">> -> deactivated
    end.
