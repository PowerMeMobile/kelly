-module(k_storage_customers).

%% API
-export([
    get_customers/0,
    get_customers/1,
    get_customers_by_dealer_uuid/1,
    get_customers_by_dealer_uuid/2,
    get_customers_uuid_by_dealer_uuid/1,
    get_customer_by_uuid/1,
    get_customer_by_id/1,
    get_customers_by_id_preffix/3,
    get_customer_by_email/1,
    get_customer_by_msisdn/1,
    set_customer/2,
    del_customer/1,

    get_user_by_id/2,
    get_user_by_email/2,
    get_user_by_msisdn/2,
    set_user/2,
    del_user/2,

    get_originator/2,
    set_originator/2,
    del_originator_by_id/2,
    del_originator_by_msisdn/2,

    delete_dealer_customers/1,
    deactivate_dealer_customers/1,
    block_dealer_customers/1,
    unblock_dealer_customers/1,

    get_next_vacant_customer_id/0,

    change_credit/2,
    transfer_credit/4
]).

-include("storages.hrl").
-include("customer.hrl").
-include("dealer.hrl").
-include_lib("alley_common/include/logging.hrl").


%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_uuid(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerUuid, Customer) ->
    OriginatorsDocList = [
        {
            'id'         , O#originator.id,
            'address' , {
                'addr', (O#originator.address)#addr.addr,
                'ton' , (O#originator.address)#addr.ton,
                'npi' , (O#originator.address)#addr.npi
            },
            'description', O#originator.description,
            'is_default' , O#originator.is_default,
            'routings'   , routings_to_docs(O#originator.routings),
            'state'      , bsondoc:atom_to_binary(O#originator.state)
        }
        || O <- Customer#customer.originators
    ],

    UsersDocList = [
        {
            'id'          , U#user.id,
            'password'    , U#user.password,
            'interfaces'  ,
                [bsondoc:atom_to_binary(I) || I <- U#user.interfaces],
            'features'    , features_to_docs(U#user.features),
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
        '$setOnInsert', {
            'customer_id'        , Customer#customer.customer_id,
            'dealer_id'          , Customer#customer.dealer_id,
            'credit'             , Customer#customer.credit
        },
        '$set', {
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
            'interfaces',
                [bsondoc:atom_to_binary(I) || I <- Customer#customer.interfaces],
            'features'           , features_to_docs(Customer#customer.features),
            'pay_type'           , bsondoc:atom_to_binary(Customer#customer.pay_type),
            'credit_limit'       , Customer#customer.credit_limit,
            'language'           , Customer#customer.language,
            'state'              , bsondoc:atom_to_binary(Customer#customer.state)
        }
    },
    case mongodb_storage:upsert(static_storage, customers, {'_id', CustomerUuid}, Modifier) of
        ok ->
            ok = k_event_manager:notify_customer_changed(CustomerUuid);
        {error, Reason} ->
            {error, Reason}
    end.


-spec get_customers() -> {ok, [#customer{}]} | {error, term()}.
get_customers() ->
    get_customers(undefined).


-spec get_customers(customer_state() | undefined) -> {ok, [#customer{}]} | {error, term()}.
get_customers(State) when
        (State =:= active orelse
        State =:= blocked orelse
        State =:= deactivated orelse
        State =:= undefined) ->
    StateNotEqualDeletedSelector = {'state', {'$ne', ?DELETED_ST}},
    Selector =
    case State of
        undefined ->
            StateNotEqualDeletedSelector;
        _ ->
            {'$and', [StateNotEqualDeletedSelector, {'state', atom_to_binary(State, latin1)}]}
    end,
    case mongodb_storage:find(static_storage, customers, Selector) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        {error, Reason} ->
            {error, Reason}
    end.


get_customers_by_dealer_uuid(DealerUUID) ->
    get_customers_by_dealer_uuid(DealerUUID, undefined).

-spec get_customers_by_dealer_uuid(dealer_uuid(), customer_state() | undefined) ->
    {ok, [#customer{}]} | {error, term()}.
get_customers_by_dealer_uuid(DealerUUID, State) when
        is_binary(DealerUUID) andalso
            (State =:= active orelse
            State =:= blocked orelse
            State =:= deactivated orelse
            State =:= undefined) ->

    BaseSelectorList = [
        'dealer_id', DealerUUID
    ],

    StateSelectorList =
    case State of
        undefined ->
            ['state', {'$ne', ?DELETED_ST}];
        _ ->
            ['$and', [{'state', atom_to_binary(State, latin1)}, {'state', {'$ne', ?DELETED_ST}}]]
    end,

    SelectorList = StateSelectorList ++ BaseSelectorList,

    Selector = list_to_tuple(SelectorList),
    case mongodb_storage:find(static_storage, customers, Selector) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        {error, Reason} ->
            {error, Reason}
    end.


-spec get_customers_uuid_by_dealer_uuid(dealer_uuid()) -> {ok, [customer_uuid()]}.
get_customers_uuid_by_dealer_uuid(DealerUuid) when is_binary(DealerUuid) ->
    {ok, DealerCustomers} = get_customers_by_dealer_uuid(DealerUuid),
    DealerCustomersUuidList = [C#customer.customer_uuid || C <- DealerCustomers],
    {ok, DealerCustomersUuidList}.

-spec get_customer_by_uuid(customer_uuid()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_uuid(CustomerUuid) ->
    Selector = {
        '_id', CustomerUuid,
        'state', {'$ne', ?DELETED_ST}
    },
    case mongodb_storage:find_one(static_storage, customers, Selector) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customer_by_id(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_id(CustomerId) ->
    Selector = {
        'customer_id', CustomerId,
        'state', {'$ne', ?DELETED_ST}
    },
    case mongodb_storage:find_one(static_storage, customers, Selector) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customers_by_id_preffix(dealer_id() | undefined, binary(), customer_state() | undefined) ->
    {ok, [customer()]} | {error, term()}.
get_customers_by_id_preffix(DealerId, CustomerIdPreffix, State) when
        is_binary(CustomerIdPreffix) andalso
            (State =:= active orelse
            State =:= blocked orelse
            State =:= deactivated orelse
            State =:= undefined) ->

    Regex = <<"^", CustomerIdPreffix/binary>>,

    SelectorList0 = [
        'customer_id', {'$regex', Regex, '$options', <<"i">>}
    ],

    StateNotEqualDeletedSelectorList = ['state', {'$ne', ?DELETED_ST}],

    SelectorList1 =
    case State of
        undefined ->
            StateNotEqualDeletedSelectorList ++ SelectorList0;
        _ ->
            ['$and', [
                list_to_tuple(StateNotEqualDeletedSelectorList),
                {'state', atom_to_binary(State, latin1)}
            ]] ++ SelectorList0
    end,

    SelectorList =
    if
        DealerId =/= undefined ->
            ['dealer_id', DealerId | SelectorList1];
        true ->
            SelectorList1
    end,

    Selector = list_to_tuple(SelectorList),
    case mongodb_storage:find(static_storage, customers, Selector) of
        {ok, DocList} ->
            {ok, [doc_to_record(Doc) || {_, Doc} <- DocList]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customer_by_email(email()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_email(Email) ->
    Selector = {
        'users.email', Email,
        'state', {'$ne', ?DELETED_ST}
    },
    case mongodb_storage:find_one(static_storage, customers, Selector) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_customer_by_msisdn(addr()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_msisdn(Msisdn) ->
    Selector = {
        'users.mobile_phone', Msisdn#addr.addr,
        'state', {'$ne', ?DELETED_ST}
    },
    case mongodb_storage:find_one(static_storage, customers, Selector) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec del_customer(customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{}} ->
            case mongodb_storage:delete(static_storage, customers, {'_id', CustomerUuid}) of
                ok ->
                    ok = k_event_manager:notify_customer_changed(CustomerUuid);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, no_entry} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_user_by_id(#customer{}, user_id()) -> {ok, #user{}} | {error, no_entry}.
get_user_by_id(#customer{users = Users}, UserId) ->
    find(UserId, #user.id, Users).

-spec get_user_by_email(#customer{}, email()) -> {ok, #user{}} | {error, no_entry}.
get_user_by_email(#customer{users = Users}, Email) ->
    find(Email, #user.email, Users).

-spec get_user_by_msisdn(#customer{}, addr()) -> {ok, #user{}} | {error, no_entry}.
get_user_by_msisdn(#customer{users = Users}, Msisdn) ->
    find(Msisdn#addr.addr, #user.mobile_phone, Users).

-spec set_user(#user{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_user(User = #user{id = UserId}, CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = lists:keydelete(UserId, #user.id, Users),
            set_customer(CustomerUuid, Customer#customer{users = [User | NewUsers]});
        {error, Reason} ->
            {error, Reason}
    end.

-spec del_user(customer_uuid(), user_id()) -> ok | {error, no_entry} | {error, term()}.
del_user(CustomerUuid, UserId) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{users = Users}} ->
            NewUsers = lists:keydelete(UserId, #user.id, Users),
            set_customer(CustomerUuid, Customer#customer{users = NewUsers});
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_originator(#customer{}, originator_id()) -> {ok, #originator{}} | {error, no_entry}.
get_originator(#customer{originators = Originators}, OriginatorId) ->
    find(OriginatorId, #originator.id, Originators).

-spec set_originator(#originator{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_originator(Originator = #originator{id = OriginatorId}, CustomerUuid) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{originators = Originators}} ->
            NewOriginators = lists:keydelete(OriginatorId, #originator.id, Originators),
            set_customer(CustomerUuid, Customer#customer{originators = [Originator | NewOriginators]});
        {error, Reason} ->
            {error, Reason}
    end.

-spec del_originator_by_id(customer_uuid(), originator_id()) ->
    ok | {error, no_entry} | {error, term()}.
del_originator_by_id(CustomerUuid, OriginatorId) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{originators = Originators}} ->
            NewOriginators = lists:keydelete(OriginatorId, #originator.id, Originators),
            set_customer(CustomerUuid, Customer#customer{originators = NewOriginators});
        {error, Reason} ->
            {error, Reason}
    end.

-spec del_originator_by_msisdn(customer_uuid(), addr()) ->
    ok | {error, no_entry} | {error, term()}.
del_originator_by_msisdn(CustomerUuid, Msisdn) ->
    case get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{originators = Originators}} ->
            NewOriginators = lists:keydelete(Msisdn, #originator.address, Originators),
            set_customer(CustomerUuid, Customer#customer{originators = NewOriginators});
        {error, Reason} ->
            {error, Reason}
    end.


-spec get_next_vacant_customer_id() -> {ok, customer_id()} | {error, term()}.
get_next_vacant_customer_id() ->
    Selector = {},
    Projector = {'customer_id', 1},
    case mongodb_storage:find(static_storage, customers, Selector, Projector) of
        {ok, CustomerIdDocList} ->
            get_next_vacant_customer_id(CustomerIdDocList);
        {error, Reason} ->
            {error, Reason}
    end.

get_next_vacant_customer_id(CustomerIdDocList) ->
    CustomerIdList = format_customer_id_list(CustomerIdDocList),
    get_next_vacant_customer_id(CustomerIdList, 1).

get_next_vacant_customer_id(CustomerIdList, VacantCustomerIdInt) ->
    VacantCustomerId = list_to_binary(integer_to_list(VacantCustomerIdInt)),
    case lists:member(VacantCustomerId, CustomerIdList) of
        true ->
            get_next_vacant_customer_id(CustomerIdList, VacantCustomerIdInt + 1);
        false ->
            {ok, VacantCustomerId}
    end.

format_customer_id_list(CustomerIdDocList) ->
    [begin
        bsondoc:at('customer_id', Doc)
    end || {_Id, Doc} <- CustomerIdDocList].


-spec change_credit(customer_uuid(), float()) ->
    {ok, float()} | {error, term()}.
change_credit(CustomerUuid, Amount) ->
    change_credit(CustomerUuid, Amount, undefined, false).

change_credit(CustomerUuid, Amount, TransactionUuid, CheckCreditAvailable) ->
    Update =
    case TransactionUuid of
        undefined ->
            {'$inc', {'credit', Amount}};
        _ ->
            {'$inc', {'credit', Amount}, '$push', {'transactions', TransactionUuid}}
    end,

    Query =
    if
        CheckCreditAvailable ->
            {'_id', CustomerUuid ,'credit', {'$gte', (-1 * Amount)}};
        true ->
            {'_id', CustomerUuid}
    end,

    Command = {
        'findandmodify', <<"customers">>,
        'query' , Query,
        'update', Update,
        'fields', {'credit', 1},
        'new'   , true
    },
    case mongodb_storage:command(static_storage, Command) of
        {ok, Result} ->
            case bsondoc:at(value, Result) of
                undefined ->
                    {error, no_entry};
                Value ->
                    %% Do not notify customer changed here.
                    %% It will clear the auth cache on every change.
                    {ok, bsondoc:at(credit, Value)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


-spec transfer_credit(dealer_uuid(), customer_uuid(), customer_uuid(), float()) -> ok.
transfer_credit(DealerUuid, FromCustomerUuid, ToCustomerUuid, Amount) ->
    Props = [
        {dealer_uuid, DealerUuid},
        {from_customer_uuid, FromCustomerUuid},
        {to_customer_uuid, ToCustomerUuid},
        {amount, Amount}
    ],
    transfer_credit(check_from_customer, Props).

transfer_credit(check_from_customer, Props0) ->
    FromCustomerUuid = proplists:get_value(from_customer_uuid, Props0),
    DealerUuid = proplists:get_value(dealer_uuid, Props0),
    Amount = proplists:get_value(amount, Props0),
    case get_customer_by_uuid(FromCustomerUuid) of
        {ok, #customer{dealer_id = DealerUuid, credit = Credit}} when
                Credit >= Amount ->
            transfer_credit(check_to_customer, Props0);
        {ok, #customer{dealer_id = DealerUuid}} ->
            {error, not_enough_credit_amount};
        {ok, #customer{}} ->
            {error, from_customer_not_belong_to_dealer};
        {error, no_entry} ->
            {error, from_customer_doesnot_exist};
        {error, Error} ->
            {error, cant_fetch_from_customer, Error}
    end;

transfer_credit(check_to_customer, Props0) ->
    ToCustomerUuid = proplists:get_value(to_customer_uuid, Props0),
    DealerUuid = proplists:get_value(dealer_uuid, Props0),
    case get_customer_by_uuid(ToCustomerUuid) of
        {ok, #customer{dealer_id = DealerUuid}} ->
            transfer_credit(change_credit_from, Props0);
        {ok, #customer{}} ->
            {error, to_customer_not_belong_to_dealer};
        {error, no_entry} ->
            {error, to_customer_doesnot_exist};
        {error, Error} ->
            {error, cant_fetch_to_customer, Error}
    end;

transfer_credit(create_transaction, Props0) ->
    ToCustomerUuid = proplists:get_value(to_customer_uuid, Props0),
    FromCustomerUuid = proplists:get_value(from_customer_uuid, Props0),
    Amount = proplists:get_value(amount, Props0),
    DealerUuid = proplists:get_value(dealer_uuid, Props0),
    case create_transaction(DealerUuid, FromCustomerUuid, ToCustomerUuid, Amount) of
        {ok, TransactionUuid} ->
            Props1 = [{transaction_uuid, TransactionUuid} | Props0],
            transfer_credit(change_credit_from, Props1);
        {error, Error} ->
            {error, cant_create_transaction, Error}
    end;

transfer_credit(change_credit_from, Props0) ->
    TransactionUuid = proplists:get_value(transaction_uuid, Props0),
    FromCustomerUuid = proplists:get_value(from_customer_uuid, Props0),
    Amount = proplists:get_value(amount, Props0),
    case change_credit(FromCustomerUuid, (Amount * -1), TransactionUuid, _CheckCreditAvailable = true) of
        {ok, _} ->
            transfer_credit(change_credit_to, Props0);
        {error, no_entry} ->
            {error, TransactionUuid, not_enough_credits_or_customer_not_exist};
        {error, Error} ->
            {error, TransactionUuid, cant_change_credit_for_from_customer, Error}
    end;

transfer_credit(change_credit_to, Props0) ->
    TransactionUuid = proplists:get_value(transaction_uuid, Props0),
    ToCustomerUuid = proplists:get_value(to_customer_uuid, Props0),
    Amount = proplists:get_value(amount, Props0),
    case change_credit(ToCustomerUuid, Amount, TransactionUuid, _CheckCreditAvailable = false) of
        {ok, _} ->
            transfer_credit(complete_transaction, Props0);
        {error, Error} ->
            {error, TransactionUuid, cant_change_credit_for_to_customer, Error}
    end;

transfer_credit(complete_transaction, Props) ->
    TransactionUuid = proplists:get_value(transaction_uuid, Props),
    case complete_transaction(TransactionUuid) of
        ok ->
            {ok, TransactionUuid};
        {error, Error} ->
            {error, TransactionUuid, cant_complete_transaction, Error}
    end.


create_transaction(DealerUuid, FromCustomerUuid, ToCustomerUuid, Amount) ->
    TransactionUuid = uuid:unparse(uuid:generate_time()),
    Modifier = {
        '_id', TransactionUuid,
        'dealer_uuid', DealerUuid,
        'from', FromCustomerUuid,
        'to', ToCustomerUuid,
        'amount', Amount,
        'state', 'in_progress',
        'time', now()
    },
    case mongodb_storage:insert(static_storage, credit_transactions, Modifier) of
        {ok, TransactionUuid} -> {ok, TransactionUuid};
        {error, Reason} -> {error, Reason}
    end.


complete_transaction(TransactionUuid) ->
    Selector = {
        '_id', TransactionUuid
    },
    Modifier = {
        '$set', {'state', 'completed'}
    },
    case mongodb_storage:update(static_storage, credit_transactions, Selector, Modifier) of
        ok -> ok;
        {error, Reason} ->
            {error, Reason}
    end.


-spec delete_dealer_customers(dealer_uuid()) -> ok | {error, term()}.
delete_dealer_customers(DealerUUID) when is_binary(DealerUUID) ->
    {ok, Customers} = get_customers_by_dealer_uuid(DealerUUID),
    delete_dealer_customers(DealerUUID, Customers).
delete_dealer_customers(_, []) -> ok;
delete_dealer_customers(DealerUUID, [Customer | OtherCustomers]) ->
    CustomerUUID = Customer#customer.customer_uuid,
    Selector = {'_id', CustomerUUID},
    PrevState = bsondoc:atom_to_binary(Customer#customer.state),
    NewState = bsondoc:atom_to_binary(?DELETED_ST),
    Modifier = {'$set', {
        'state', NewState,
        'prev_state', PrevState
    }},
    case mongodb_storage:update(static_storage, customers, Selector, Modifier) of
        ok ->
            ok = k_event_manager:notify_customer_changed(CustomerUUID),
            delete_dealer_customers(DealerUUID, OtherCustomers);
        {error, Reason} ->
            {error, Reason}
    end.


-spec deactivate_dealer_customers(dealer_uuid()) -> ok | {error, term()}.
deactivate_dealer_customers(DealerUUID) when is_binary(DealerUUID) ->
    {ok, Customers} = get_customers_by_dealer_uuid(DealerUUID),
    deactivate_dealer_customers(DealerUUID, Customers).
deactivate_dealer_customers(_, []) -> ok;
deactivate_dealer_customers(DealerUUID, [Customer | OtherCustomers]) ->
    CustomerUUID = Customer#customer.customer_uuid,
    Selector = {'_id', CustomerUUID},
    PrevState = bsondoc:atom_to_binary(Customer#customer.state),
    NewState = bsondoc:atom_to_binary(?DEACTIVATED_ST),
    Modifier = {'$set', {
        'state', NewState,
        'prev_state', PrevState
    }},
    case mongodb_storage:update(static_storage, customers, Selector, Modifier) of
        ok ->
            ok = k_event_manager:notify_customer_changed(CustomerUUID),
            deactivate_dealer_customers(DealerUUID, OtherCustomers);
        {error, Reason} ->
            {error, Reason}
    end.


-spec block_dealer_customers(dealer_uuid()) -> ok | {error, term()}.
block_dealer_customers(DealerUUID) when is_binary(DealerUUID) ->
    {ok, Customers} = get_customers_by_dealer_uuid(DealerUUID),
    block_dealer_customers(DealerUUID, Customers).
block_dealer_customers(_, []) -> ok;
block_dealer_customers(DealerUUID, [Customer | OtherCustomers]) ->
    CustomerUUID = Customer#customer.customer_uuid,
    Selector = {'_id', CustomerUUID},
    PrevState = bsondoc:atom_to_binary(Customer#customer.state),
    NewState = bsondoc:atom_to_binary(?BLOCKED_ST),
    Modifier = {'$set', {
        'state', NewState,
        'prev_state', PrevState
    }},
    case mongodb_storage:update(static_storage, customers, Selector, Modifier) of
        ok ->
            ok = k_event_manager:notify_customer_changed(CustomerUUID),
            block_dealer_customers(DealerUUID, OtherCustomers);
        {error, Reason} ->
            {error, Reason}
    end.


-spec unblock_dealer_customers(dealer_uuid()) -> ok | {error, term()}.
unblock_dealer_customers(DealerUUID) when is_binary(DealerUUID) ->
    {ok, Customers} = get_customers_by_dealer_uuid(DealerUUID),
    unblock_dealer_customers(DealerUUID, Customers).
unblock_dealer_customers(_, []) -> ok;
unblock_dealer_customers(DealerUUID, [Customer | OtherCustomers]) ->
    CustomerUUID = Customer#customer.customer_uuid,

    PrevState =
    case get_customer_previous_state(CustomerUUID) of
        {ok, undefined} -> ?ACTIVE_ST;
        {ok, PS} -> PS
    end,

    Selector = {'_id', CustomerUUID},
    Modifier = {'$set', {
        'state', PrevState
    }, '$unset', {'prev_state', <<>>}},
    case mongodb_storage:update(static_storage, customers, Selector, Modifier) of
        ok ->
            ok = k_event_manager:notify_customer_changed(CustomerUUID),
            unblock_dealer_customers(DealerUUID, OtherCustomers);
        {error, Reason} ->
            {error, Reason}
    end.


get_customer_previous_state(CustomerUUID) ->
    Selector = {
        '_id', CustomerUUID
    },
    case mongodb_storage:find_one(static_storage, customers, Selector) of
        {ok, Doc} ->
            PrevState = bsondoc:at(prev_state, Doc),
            {ok, PrevState};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal
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
            routings = docs_to_routings(bsondoc:at(routings, OrigDoc)),
            state = bsondoc:binary_to_atom(bsondoc:at(state, OrigDoc))
        }
        || OrigDoc <- OriginatorsDocs
    ],

    UsersDocs = bsondoc:at(users, Doc),
    Users = [
        #user{
            id = bsondoc:at(id, UserDoc),
            password = bsondoc:at(password, UserDoc),
            interfaces =
                [bsondoc:binary_to_atom(I) || I <- bsondoc:at(interfaces, UserDoc, [])],
            features = docs_to_features(bsondoc:at(features, UserDoc)),
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
    DealerId = bsondoc:at(dealer_id, Doc),
    Name = bsondoc:at(name, Doc),
    Priority = bsondoc:at(priority, Doc),
    RPS = bsondoc:at(rps, Doc),
    NetMapId = bsondoc:at(network_map_id, Doc),
    DefProvId = bsondoc:at(default_provider_id, Doc),
    ReceiptsAllowed = bsondoc:at(receipts_allowed, Doc),
    NoRetry = bsondoc:at(no_retry, Doc),
    DefValidity = bsondoc:at(default_validity, Doc),
    MaxValidity = bsondoc:at(max_validity, Doc),
    Interfaces =
        [bsondoc:binary_to_atom(I) || I <- bsondoc:at(interfaces, Doc, [])],
    Features = docs_to_features(bsondoc:at(features, Doc, [])),
    PayType = bsondoc:binary_to_atom(bsondoc:at(pay_type, Doc)),
    Credit = bsondoc:at(credit, Doc),
    CreditLimit = bsondoc:at(credit_limit, Doc),
    Language = bsondoc:at(language, Doc),
    State = bsondoc:binary_to_atom(bsondoc:at(state, Doc)),

    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        dealer_id = DealerId,
        name = Name,
        priority = Priority,
        rps = RPS,
        originators = Originators,
        network_map_id = NetMapId,
        default_provider_id = check_undefined(DefProvId),
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefValidity,
        max_validity = MaxValidity,
        users = Users,
        interfaces = Interfaces,
        features = Features,
        pay_type = PayType,
        credit = Credit,
        credit_limit = CreditLimit,
        language = Language,
        state = State
    }.

-spec find(binary(), integer(), list()) -> {ok, term()} | {error, no_entry}.
find(Key, Pos, List) ->
    case lists:keyfind(Key, Pos, List) of
        false ->
            {error, no_entry};
        Item ->
            {ok, Item}
    end.

check_undefined(<<>>) ->
    undefined;
check_undefined(Value) ->
    Value.

features_to_docs(undefined) ->
    [];
features_to_docs(Features) ->
    [{'name', N, 'value', V} || #feature{name = N, value = V} <- Features].

docs_to_features(undefined) ->
    [];
docs_to_features(Docs) ->
    [#feature{name = bsondoc:at(name, D), value = bsondoc:at(value, D)} || D <- Docs].

routings_to_docs(undefined) ->
    [];
routings_to_docs(Routings) ->
    [{'network_map_id', NMI, 'default_provider_id', DPI} ||
     #routing{network_map_id = NMI, default_provider_id = DPI} <- Routings].

docs_to_routings(undefined) ->
    [];
docs_to_routings(Docs) ->
    [#routing{network_map_id = NMI2, default_provider_id = DPI2} ||
     {'network_map_id', NMI, 'default_provider_id', DPI} <- Docs,
     begin
        NMI2 = check_undefined(NMI),
        DPI2 = check_undefined(DPI),
        NMI2 =/= undefined
     end].
