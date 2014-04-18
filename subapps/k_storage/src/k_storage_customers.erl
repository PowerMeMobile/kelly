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
    del_customer_user/2
]).

-include("storages.hrl").
-include("customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_uuid(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerUuid, Customer) ->
    AllowedSourcesDocList = [
        {'addr' , Addr#addr.addr , 'ton' , Addr#addr.ton , 'npi' , Addr#addr.npi}
        || Addr <- Customer#customer.allowed_sources
    ],

    DefaultSourceDoc =
        case Customer#customer.default_source of
            undefined -> undefined;
            Addr = #addr{} ->
                {'addr' , Addr#addr.addr , 'ton' , Addr#addr.ton , 'npi' , Addr#addr.npi}
        end,

    UsersDocList = [
        {
            'id' , User#user.id,
            'password'  , User#user.password,
            'connection_types',
                [bsondoc:atom_to_binary(Type) || Type <- User#user.connection_types]
        }
        || User <- Customer#customer.users
    ],

    Modifier = {
        '$set' , {
            'customer_id'         , Customer#customer.customer_id,
            'name'                , Customer#customer.name,
            'priority'            , Customer#customer.priority,
            'rps'                 , Customer#customer.rps,
            'allowed_sources'     , AllowedSourcesDocList,
            'default_source'      , DefaultSourceDoc,
            'network_map_id'      , Customer#customer.network_map_id,
            'default_provider_id' , Customer#customer.default_provider_id,
            'receipts_allowed'    , Customer#customer.receipts_allowed,
            'no_retry'            , Customer#customer.no_retry,
            'default_validity'    , Customer#customer.default_validity,
            'max_validity'        , Customer#customer.max_validity,
            'users'               , UsersDocList,
            'pay_type'            , bsondoc:atom_to_binary(Customer#customer.pay_type),
            'credit'              , Customer#customer.credit,
            'credit_limit'        , Customer#customer.credit_limit,
            'state'               , Customer#customer.state
        }
    },
    mongodb_storage:upsert(static_storage, customers, {'_id', CustomerUuid}, Modifier).

-spec get_customers() -> {ok, [{customer_uuid(), #customer{}}]} | {error, term()}.
get_customers() ->
    case mongodb_storage:find(static_storage, customers, {}) of
        {ok, List} ->
            {ok, [
                {Id, doc_to_record(Doc)} || {Id, Doc} <- List
            ]};
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
    mongodb_storage:delete(static_storage, customers, {'_id', CustomerUuid}).

-spec get_customer_user(#customer{}, UserId :: string()) -> {ok, #user{}} | {error, no_entry} | {error, term()}.
get_customer_user(#customer{users = UserList}, UserId) ->
    find_user(UserList, UserId).

-spec set_customer_user(#user{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_customer_user(User = #user{id = UserId}, CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = lists:keydelete(UserId, #user.id, Users),
            set_customer(CustomerUuid, Customer#customer{users = [User | NewUsers]});
        Error -> Error
    end.

-spec del_customer_user(customer_uuid(), user_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer_user(CustomerUuid, UserId) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = delete_user(Users, UserId),
            set_customer(CustomerUuid, Customer#customer{users = NewUsers});
        Error ->
            Error
    end.

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    UsersDocs = bsondoc:at(users, Doc),
    Users = [
        #user{
            id = bsondoc:at(id, UserDoc),
            password = bsondoc:at(password, UserDoc),
            connection_types =
                [bsondoc:binary_to_atom(Type) || Type <- bsondoc:at(connection_types, UserDoc)]
        } || UserDoc <- UsersDocs
    ],

    AllowedSourcesDocs = bsondoc:at(allowed_sources, Doc),
    AllowedSources = [
        #addr{
            addr = bsondoc:at(addr, Addr),
            ton = bsondoc:at(ton, Addr),
            npi = bsondoc:at(npi, Addr)
        }
        || Addr <- AllowedSourcesDocs],

    DefaultSource =
        case bsondoc:at(default_source, Doc) of
            undefined -> undefined;
            AddrDoc when is_tuple(AddrDoc) ->
                #addr{
                    addr = bsondoc:at(addr, AddrDoc),
                    ton = bsondoc:at(ton, AddrDoc),
                    npi = bsondoc:at(npi, AddrDoc)
                }
        end,

    CustomerUuid = bsondoc:at('_id', Doc),
    CustomerId = bsondoc:at(customer_id, Doc),
    Name = bsondoc:at(name, Doc),
    Priority = bsondoc:at(priority, Doc),
    RPS = bsondoc:at(rps, Doc),
    NetworkMapId = bsondoc:at(network_map_id, Doc),
    DefProviderId = bsondoc:at(default_provider_id, Doc),
    ReceiptsAllowed = bsondoc:at(receipts_allowed, Doc),
    NoRetry = bsondoc:at(no_retry, Doc),
    DefValidity = bsondoc:at(default_validity, Doc),
    MaxValidity = bsondoc:at(max_validity, Doc),
    PayType = bsondoc:binary_to_atom(bsondoc:at(pay_type, Doc)),
    Credit = bsondoc:at(credit, Doc),
    CreditLimit = bsondoc:at(credit_limit, Doc),
    State = bsondoc:at(state, Doc),

    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        name = Name,
        priority = Priority,
        rps = RPS,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
        network_map_id = NetworkMapId,
        default_provider_id = DefProviderId,
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefValidity,
        max_validity = MaxValidity,
        users = Users,
        pay_type = PayType,
        credit = Credit,
        credit_limit = CreditLimit,
        state = State
    }.

%% ===================================================================
%% Internal
%% ===================================================================

-spec find_user([#user{}], string()) -> {ok, #user{}} | {error, no_entry}.
find_user([], _Uname) ->
    {error, no_entry};
find_user([User = #user{id = Uname} | _RestUsers], Uname) ->
    {ok, User};
find_user([_User | RestUsers], UserName) ->
    find_user(RestUsers, UserName).

delete_user(Users, UserId) ->
    lists:foldl(
        fun(CurrentUser = #user{id = CurrentUserId}, Acc)->
            case CurrentUserId of
                UserId -> Acc;
                _Any -> [CurrentUser | Acc]
            end
        end, [], Users
    ).
