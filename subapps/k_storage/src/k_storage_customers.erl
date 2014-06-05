-module(k_storage_customers).

%% API
-export([
    get_customers/0,
    get_customer_by_uuid/1,
    get_customer_by_id/1,
    set_customer/2,
    del_customer/1,
    get_customer_user/2,
    set_customer_user/2,
    del_customer_user/2,
    get_customer_originator/2,
    set_customer_originator/2,
    del_customer_originator/2,
    reduce_credit/2
]).

-include("storages.hrl").
-include("customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_uuid(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerUuid, Customer) ->
    OriginatorsDocList = [
        {
            'id'     , O#originator.id,
            'address', {
                'addr', (O#originator.address)#addr.addr,
                'ton' , (O#originator.address)#addr.ton,
                'npi' , (O#originator.address)#addr.npi
            },
            'description', O#originator.description,
            'is_default' , O#originator.is_default,
            'state'      , bsondoc:atom_to_binary(O#originator.state)
        }
        || O <- Customer#customer.originators
    ],

    UsersDocList = [
        {
            'id' , U#user.id,
            'password', U#user.password,
            'connection_types',
                [bsondoc:atom_to_binary(Type) || Type <- U#user.connection_types],
            'mobile_phone', U#user.mobile_phone,
            'first_name'  , U#user.first_name,
            'last_name'   , U#user.last_name,
            'company'     , U#user.company,
            'occupation'  , U#user.occupation,
            'email'       , U#user.email,
            'country'     , U#user.country,
            'language'    , U#user.language,
            'state'       , bsondoc:atom_to_binary(U#user.state)
        }
        || U <- Customer#customer.users
    ],

    Modifier = {
        '$set' , {
            'customer_id'        , Customer#customer.customer_id,
            'name'               , Customer#customer.name,
            'priority'           , Customer#customer.priority,
            'rps'                , Customer#customer.rps,
            'originators'        , OriginatorsDocList,
            'network_map_id'     , Customer#customer.network_map_id,
            'default_provider_id', Customer#customer.default_provider_id,
            'receipts_allowed'   , Customer#customer.receipts_allowed,
            'no_retry'           , Customer#customer.no_retry,
            'default_validity'   , Customer#customer.default_validity,
            'max_validity'       , Customer#customer.max_validity,
            'users'              , UsersDocList,
            'pay_type'           , bsondoc:atom_to_binary(Customer#customer.pay_type),
            'credit'             , Customer#customer.credit,
            'credit_limit'       , Customer#customer.credit_limit,
            'language'           , Customer#customer.language,
            'state'              , bsondoc:atom_to_binary(Customer#customer.state)
        }
    },
    case mongodb_storage:upsert(static_storage, customers, {'_id', CustomerUuid}, Modifier) of
        ok ->
            k_event_manager:notify_customer_changed(CustomerUuid, Customer#customer.customer_id),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customers() -> {ok, [#customer{}]} | {error, term()}.
get_customers() ->
    case mongodb_storage:find(static_storage, customers, {}) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        Error ->
            Error
    end.

-spec get_customer_by_uuid(customer_uuid()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_uuid(CustomerUuid) ->
    case mongodb_storage:find_one(static_storage, customers, {'_id', CustomerUuid}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec get_customer_by_id(customer_id()) -> {ok, #customer{}} | any().
get_customer_by_id(CustomerId) ->
    case mongodb_storage:find_one(static_storage, customers, {'customer_id', CustomerId}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec del_customer(customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{customer_id = CustomerId}} ->
            case mongodb_storage:delete(static_storage, customers, {'_id', CustomerUuid}) of
                ok ->
                    k_event_manager:notify_customer_changed(CustomerUuid, CustomerId),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, no_entry} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customer_user(#customer{}, user_id()) -> {ok, #user{}} | {error, no_entry}.
get_customer_user(#customer{users = Users}, UserId) ->
    find(UserId, #user.id, Users).

-spec set_customer_user(#user{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_customer_user(User = #user{id = UserId}, CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = lists:keydelete(UserId, #user.id, Users),
            set_customer(CustomerUuid, Customer#customer{users = [User | NewUsers]});
        Error ->
            Error
    end.

-spec del_customer_user(customer_uuid(), user_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer_user(CustomerUuid, UserId) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = lists:keydelete(UserId, #user.id, Users),
            set_customer(CustomerUuid, Customer#customer{users = NewUsers});
        Error ->
            Error
    end.

-spec get_customer_originator(#customer{}, originator_id()) -> {ok, #originator{}} | {error, no_entry}.
get_customer_originator(#customer{originators = Originators}, OriginatorId) ->
    find(OriginatorId, #originator.id, Originators).

-spec set_customer_originator(#originator{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_customer_originator(Originator = #originator{id = OriginatorId}, CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{originators = Originators}} ->
            NewOriginators = lists:keydelete(OriginatorId, #originator.id, Originators),
            set_customer(CustomerUuid, Customer#customer{originators = [Originator | NewOriginators]});
        Error -> Error
    end.

-spec del_customer_originator(customer_uuid(), originator_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer_originator(CustomerUuid, OriginatorId) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{originators = Originators}} ->
            NewOriginators = lists:keydelete(OriginatorId, #originator.id, Originators),
            set_customer(CustomerUuid, Customer#customer{originators = NewOriginators});
        Error ->
            Error
    end.

-spec reduce_credit(customer_uuid(), float()) -> ok | {error, term()}.
reduce_credit(CustomerUuid, Credit) ->
    Modifier = {
        '$inc', {
            'credit', -Credit
        }
    },
    mongodb_storage:upsert(static_storage, customers, {'_id', CustomerUuid}, Modifier).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    OriginatorsDocs = bsondoc:at(originators, Doc),
    Originators = [
        #originator{
            id = bsondoc:at(id, OrigDoc),
            address = #addr{
                addr = bsondoc:at('address.addr', OrigDoc),
                ton = bsondoc:at('address.ton', OrigDoc),
                npi = bsondoc:at('address.npi', OrigDoc)
            },
            description = bsondoc:at(description, OrigDoc),
            is_default = bsondoc:at(is_default, OrigDoc),
            state = bsondoc:binary_to_atom(bsondoc:at(state, OrigDoc))
        }
        || OrigDoc <- OriginatorsDocs
    ],

    UsersDocs = bsondoc:at(users, Doc),
    Users = [
        #user{
            id = bsondoc:at(id, UserDoc),
            password = bsondoc:at(password, UserDoc),
            connection_types =
                [bsondoc:binary_to_atom(Type) || Type <- bsondoc:at(connection_types, UserDoc)],
            mobile_phone = bsondoc:at(mobile_phone, UserDoc),
            first_name = bsondoc:at(first_name, UserDoc),
            last_name = bsondoc:at(last_name, UserDoc),
            company = bsondoc:at(company, UserDoc),
            occupation = bsondoc:at(occupation, UserDoc),
            email = bsondoc:at(email, UserDoc),
            country = bsondoc:at(country, UserDoc),
            language = bsondoc:at(language, UserDoc),
            state = bsondoc:binary_to_atom(bsondoc:at(state, UserDoc))
        } || UserDoc <- UsersDocs
    ],

    CustomerUuid = bsondoc:at('_id', Doc),
    CustomerId = bsondoc:at(customer_id, Doc),
    Name = bsondoc:at(name, Doc),
    Priority = bsondoc:at(priority, Doc),
    RPS = bsondoc:at(rps, Doc),
    NetworkMapId = bsondoc:at(network_map_id, Doc),
    DefaultProviderId = bsondoc:at(default_provider_id, Doc),
    ReceiptsAllowed = bsondoc:at(receipts_allowed, Doc),
    NoRetry = bsondoc:at(no_retry, Doc),
    DefValidity = bsondoc:at(default_validity, Doc),
    MaxValidity = bsondoc:at(max_validity, Doc),
    PayType = bsondoc:binary_to_atom(bsondoc:at(pay_type, Doc)),
    Credit = bsondoc:at(credit, Doc),
    CreditLimit = bsondoc:at(credit_limit, Doc),
    Language = bsondoc:at(language, Doc),
    State = bsondoc:binary_to_atom(bsondoc:at(state, Doc)),

    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        name = Name,
        priority = Priority,
        rps = RPS,
        originators = Originators,
        network_map_id = NetworkMapId,
        default_provider_id = check_undefined(DefaultProviderId),
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefValidity,
        max_validity = MaxValidity,
        users = Users,
        pay_type = PayType,
        credit = Credit,
        credit_limit = CreditLimit,
        language = Language,
        state = State
    }.

%% ===================================================================
%% Internal
%% ===================================================================

-spec find(binary(), integer(), list()) -> {ok, term()} | {error, no_entry}.
find(Key, Pos, List) ->
    case lists:keyfind(Key, Pos, List) of
        false ->
            {error, no_entry};
        Item ->
            {ok, Item}
    end.

check_undefined(Value) ->
    case Value of
        <<>> ->
            undefined;
        _ ->
            Value
    end.