-module(k_http_api_handler_users).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

%% export helpers
-export([
    prepare_users/1
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
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = false, repeated = false, type = binary},
        #param{name = connection_types, mandatory = false, repeated = true, type =
             {custom, fun connection_type/1}},
        #param{name = mobile_phone, mandatory = false, repeated = false, type = binary},
        #param{name = first_name, mandatory = false, repeated = false, type = binary},
        #param{name = last_name, mandatory = false, repeated = false, type = binary},
        #param{name = company, mandatory = false, repeated = false, type = binary},
        #param{name = occupation, mandatory = false, repeated = false, type = binary},
        #param{name = email, mandatory = false, repeated = false, type = binary},
        #param{name = country, mandatory = false, repeated = false, type = binary},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun user_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = true, repeated = false, type = binary},
        #param{name = connection_types, mandatory = true, repeated = true, type =
            {custom, fun connection_type/1}},
        #param{name = mobile_phone, mandatory = false, repeated = false, type = binary},
        #param{name = first_name, mandatory = false, repeated = false, type = binary},
        #param{name = last_name, mandatory = false, repeated = false, type = binary},
        #param{name = company, mandatory = false, repeated = false, type = binary},
        #param{name = occupation, mandatory = false, repeated = false, type = binary},
        #param{name = email, mandatory = false, repeated = false, type = binary},
        #param{name = country, mandatory = false, repeated = false, type = binary},
        #param{name = language, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun user_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/customers/:customer_uuid/users/[:id]"
    }}.

create(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            create_user(Customer, Params);
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            get_customer_user(Customer, ?gv(id, Params));
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_user(Customer, Params);
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(id, Params),
    case k_storage_customers:del_customer_user(CustomerUuid, UserId) of
        {error, no_entry} ->
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'};
        ok ->
            {http_code, 204};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

%% ===================================================================
%% Internal & API
%% ===================================================================

-spec prepare_users(#user{}) -> {ok, [{atom(), term()}]}.
prepare_users(User = #user{features = Features}) ->
    {ok, FeaturesPlists} = k_http_api_handler_users_features:prepare_features(Features),
    Fun = ?record_to_proplist(user),
    Plist = Fun(
        User#user{features = FeaturesPlists}
    ),
    proplists:delete(password, Plist);
prepare_users(Users) when is_list(Users) ->
    {ok, [prepare_users(User) || User <- Users]}.

%% ===================================================================
%% Internal
%% ===================================================================

create_user(Customer, Params) ->
    UserId = ?gv(id, Params),
    case k_storage_customers:get_customer_user(Customer, UserId) of
        {ok, #user{}} ->
            ?log_error("User already exists: ~p", [UserId]),
            {exception, 'svc0004'};
        {error, no_entry} ->
            Password = ?gv(password, Params),
            ConnectionTypes = ?gv(connection_types, Params),
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
                connection_types = ConnectionTypes,
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
            ok = k_storage_customers:set_customer_user(User, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_users([User]),
            ?log_debug("User: ~p", [Plist]),
            {http_code, 201, Plist};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update_user(Customer, Params) ->
    UserId = ?gv(id, Params),
    case k_storage_customers:get_customer_user(Customer, UserId) of
        {ok, User} ->
            Password = resolve_pass(?gv(password, Params), User#user.password),
            ConnectionTypes = ?gv(connection_types, Params, User#user.connection_types),
            MobilePhone = ?gv(mobile_phone, Params, User#user.mobile_phone),
            FirstName = ?gv(first_name, Params, User#user.first_name),
            LastName = ?gv(last_name, Params, User#user.last_name),
            Company = ?gv(company, Params, User#user.company),
            Occupation = ?gv(occupation, Params, User#user.occupation),
            Email = ?gv(email, Params, User#user.email),
            Country = ?gv(country, Params, User#user.country),
            Language = ?gv(language, Params, User#user.language),
            State = ?gv(state, Params, User#user.state),
            Updated = #user{
                id = UserId,
                password = Password,
                connection_types = ConnectionTypes,
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
            ok = k_storage_customers:set_customer_user(Updated, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_users([Updated]),
            ?log_debug("User: ~p", [Plist]),
            {ok, Plist};
        {error, no_entry} ->
            CustomerUuid = ?gv(customer_uuid, Params),
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

get_customer_user(Customer, undefined) ->
    #customer{users = Users} = Customer,
    {ok, Plist} = prepare_users(Users),
    ?log_debug("User: ~p", [Plist]),
    {ok, Plist};
get_customer_user(Customer, UserId) ->
    case k_storage_customers:get_customer_user(Customer, UserId) of
        {ok, User} ->
            {ok, [Plist]} = prepare_users([User]),
            ?log_debug("User: ~p", [Plist]),
            {ok, Plist};
        {error, no_entry} ->
            CustomerUuid = Customer#customer.customer_uuid,
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

resolve_pass(undefined, Pass) ->
    Pass;
resolve_pass(NewPass, _Pass) ->
    ac_hexdump:binary_to_hexdump(crypto:hash(md5, NewPass), to_lower).

connection_type(Type) ->
    case Type of
        <<"mm">> -> mm;
        <<"soap">> -> soap;
        <<"oneapi">> -> oneapi;
        <<"transmitter">> -> transmitter;
        <<"receiver">> -> receiver;
        <<"transceiver">> -> transceiver
    end.

user_state(State) ->
    case State of
        <<"active">> -> active;
        <<"blocked">> -> blocked;
        <<"deactivated">> -> deactivated
    end.
