-module(k_storage_msisdns).

-export([
    create/1,
    delete/1,
    get_one/1,
    get_many/3,

    assign_to_customer/2,
    assign_to_user/3,
    unassign_from_customer/1,
    unassign_all_from_customer/1,
    unassign_from_user/1,
    unassign_all_from_user/2,
    get_assigned_to_customer/2,
    get_assigned_to_user/2
]).

-include("msisdn.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_one(msisdn()) -> {ok, #msisdn_info{}} | {error, no_entry}.
get_one(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    case mongodb_storage:find(static_storage, msisdns, Selector) of
        {ok, []} ->
            {error, no_entry};
        {ok, [{_, Doc}]} ->
            {ok, doc_to_info(Doc)}
    end.

-spec get_many(msisdn(), customer_uuid(), state()) -> {ok, [#msisdn_info{}]}.
get_many(Msisdn, CustomerUuid, State) ->
    Selector = bson:document(lists:flatten([
        [{'msisdn.a', {'$regex', <<"^", (Msisdn#addr.addr)/binary>>}} || Msisdn =/= undefined],
        [{'customer_uuid', CustomerUuid} || CustomerUuid =/= undefined],
        [{'customer_uuid', undefined} || State =:= free],
        [{'customer_uuid', {'$ne', undefined}} || State =:= used]
    ])),
    {ok, Docs} = mongodb_storage:find(static_storage, msisdns, Selector),
    {ok, [doc_to_info(D) || {_, D} <- Docs]}.

-spec get_assigned_to_customer(customer_uuid(), state()) -> {ok, [msisdn()]}.
get_assigned_to_customer(CustomerUuid, State) ->
    Selector = bson:document(lists:flatten([
        [{'customer_uuid', CustomerUuid}],
        [{'user_id', undefined} || State =:= free],
        [{'user_id', {'$ne', undefined}} || State =:= used]
    ])),
    {ok, Docs} = mongodb_storage:find(static_storage, msisdns, Selector),
    {ok, [k_storage_utils:doc_to_addr(bsondoc:at(msisdn, D)) || {_, D} <- Docs]}.

-spec get_assigned_to_user(customer_uuid(), user_id()) -> {ok, [msisdn()]}.
get_assigned_to_user(CustomerUuid, UserId) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    {ok, Docs} = mongodb_storage:find(static_storage, msisdns, Selector),
    {ok, [k_storage_utils:doc_to_addr(bsondoc:at(msisdn, D)) || {_, D} <- Docs]}.

-spec delete(msisdn()) -> ok.
delete(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    ok = mongodb_storage:delete(static_storage, msisdns, Selector).

-spec create(msisdn()) -> ok.
create(Msisdn) ->
    Modifier = {
        'msisdn'       , k_storage_utils:addr_to_doc(Msisdn),
        'customer_uuid', undefined,
        'user_id'      , undefined
    },
    {ok, _ID} = mongodb_storage:insert(static_storage, msisdns, Modifier),
    ok.

-spec assign_to_customer(msisdn(), customer_uuid()) -> ok.
assign_to_customer(Msisdn, CustomerUuid) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', CustomerUuid,
        'user_id'      , undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

-spec assign_to_user(msisdn(), customer_uuid(), user_id()) -> ok.
assign_to_user(Msisdn, CustomerUuid, UserId) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

-spec unassign_from_customer(msisdn()) -> ok.
unassign_from_customer(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', undefined,
        'user_id'      , undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

-spec unassign_all_from_customer(customer_uuid()) -> ok.
unassign_all_from_customer(CustomerUuid) ->
    Selector = {
        'customer_uuid', CustomerUuid
    },
    Modifier = {'$set', {
        'customer_uuid', undefined,
        'user_id'      , undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

-spec unassign_from_user(msisdn()) -> ok.
unassign_from_user(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'user_id', undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

-spec unassign_all_from_user(customer_uuid(), user_id()) -> ok.
unassign_all_from_user(CustomerUuid, UserId) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id', UserId
    },
    Modifier = {'$set', {
        'user_id', undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

doc_to_info(Doc) ->
    Msisdn = k_storage_utils:doc_to_addr(bsondoc:at(msisdn, Doc)),
    CustomerUuid = bsondoc:at(customer_uuid, Doc),
    UserId = bsondoc:at(user_id, Doc),
    #msisdn_info{
        msisdn = Msisdn,
        customer_uuid = CustomerUuid,
        user_id = UserId
    }.
