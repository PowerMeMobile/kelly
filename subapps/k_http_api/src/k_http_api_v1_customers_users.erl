-module(k_http_api_v1_customers_users).

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
    prepare_users/2
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
            {custom, fun interface/1}},
        #param{name = features, mandatory = false, repeated = true, type =
            {custom, fun feature/1}},
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
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = user_id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = true, repeated = false, type = binary},
        #param{name = interfaces, mandatory = true, repeated = true, type =
            {custom, fun interface/1}},
        #param{name = features, mandatory = true, repeated = true, type =
            {custom, fun feature/1}},
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
        route = "/v1/customers/:customer_uuid/users/[:user_id]"
    }}.

create(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            create_user(Customer, Params);
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            get_user_by_id(Customer, ?gv(user_id, Params));
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_user(Customer, Params);
        {error, no_entry} ->
            ?log_warn("Customer not found: ~p", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

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
%% Internal & API
%% ===================================================================

-spec prepare_users(#customer{}, #user{}) -> {ok, [{atom(), term()}]}.
prepare_users(Customer, User = #user{features = Features}) ->
    {ok, FeaturesPlists} =
        k_http_api_v1_customers_users_features:prepare_features(Features),
    UserFun = ?record_to_proplist(user),
    Plist = UserFun(User#user{features = FeaturesPlists}),
    Plist2 = [{user_id, ?gv(id, Plist)} | Plist],
    Plist3 = [{customer_uuid, Customer#customer.customer_uuid} | Plist2],
    Plist4 = [{customer_id, Customer#customer.customer_id} | Plist3],
    Plist5 = [{customer_name, Customer#customer.name} | Plist4],
    Plist6 = proplists:delete(password, Plist5),
    proplists:delete(id, Plist6);
prepare_users(Customer, Users) when is_list(Users) ->
    {ok, [prepare_users(Customer, User) || User <- Users]}.

%% ===================================================================
%% Internal
%% ===================================================================

create_user(Customer, Params) ->
    UserId = ?gv(user_id, Params),
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, #user{}} ->
            ?log_error("User already exists: ~p", [UserId]),
            {exception, 'svc0004'};
        {error, no_entry} ->
            Password = ?gv(password, Params),
            Interfaces = ?gv(interfaces, Params),
            Features = ?gv(features, Params),
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
            ok = k_storage_customers:set_user(User, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_users(Customer, [User]),
            ?log_debug("User: ~p", [Plist]),
            {http_code, 201, Plist}
    end.

update_user(Customer, Params) ->
    UserId = ?gv(user_id, Params),
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, User} ->
            Password = resolve_pass(?gv(password, Params), User#user.password),
            Interfaces = ?gv(interfaces, Params, User#user.interfaces),
            Features = ?gv(features, Params, User#user.features),
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
            ok = k_storage_customers:set_user(Updated, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_users(Customer, [Updated]),
            ?log_debug("User: ~p", [Plist]),
            {ok, Plist};
        {error, no_entry} ->
            CustomerUuid = ?gv(customer_uuid, Params),
            ?log_warn("User not found: (customer_uuid: ~p, user_id: ~p)", [CustomerUuid, UserId]),
            {exception, 'svc0003'}
    end.

get_user_by_id(Customer, undefined) ->
    #customer{users = Users} = Customer,
    {ok, Plist} = prepare_users(Customer, Users),
    ?log_debug("User: ~p", [Plist]),
    {ok, Plist};
get_user_by_id(Customer, UserId) ->
    case k_storage_customers:get_user_by_id(Customer, UserId) of
        {ok, User} ->
            {ok, [Plist]} = prepare_users(Customer, [User]),
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

interface(<<"transmitter">>) -> transmitter;
interface(<<"receiver">>)    -> receiver;
interface(<<"transceiver">>) -> transceiver;
interface(<<"soap">>)        -> soap;
interface(<<"mm">>)          -> mm;
interface(<<"oneapi">>)      -> oneapi;
interface(<<"email">>)       -> email.

features() -> [
    {<<"override_originator">>, [<<"empty">>, <<"any">>, <<"false">>]},
    {<<"inbox">>, [<<"true">>, <<"false">>]},
    {<<"sms_from_email">>, [<<"true">>, <<"false">>]}
].

feature(Binary) ->
    [Name, Value] = binary:split(Binary, <<",">>),
    case proplists:get_value(Name, features()) of
        undefined ->
            error(unknown_feature_name);
        Values ->
            case lists:member(Value, Values) of
                false ->
                    error(unknown_feature_value);
                true ->
                    #feature{name = Name, value = Value}
            end
    end.

user_state(<<"active">>)      -> active;
user_state(<<"blocked">>)     -> blocked;
user_state(<<"deactivated">>) -> deactivated.
