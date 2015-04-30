-module(k_storage_msisdns).

-export([
    create/1,
    delete/1,
    get_one/1,
    get_many/3,

    assign_to_customer/2,
    assign_to_user/3,
    unassign_from_customer/1,
    unassign_from_user/1,
    get_assigned_to_customer/1,
    get_assigned_to_user/2
]).

-include("storages.hrl").
-include("customer.hrl").

-type msisdn() :: addr().
-type state() :: all | free | used.

%% ===================================================================
%% API
%% ===================================================================

-spec get_one(msisdn()) -> {ok, {msisdn(), customer_uuid()}} | {error, not_found}.
get_one(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    case mongodb_storage:find(static_storage, msisdns, Selector) of
        {ok, []} ->
            {error, not_found};
        {ok, [{_, Doc}]} ->
            {ok, doc_to_tuple(Doc)}
    end.

-spec get_many(msisdn(), customer_uuid(), state()) -> {ok, [{msisdn(), customer_uuid()}]}.
get_many(Msisdn, CustomerUuid, State) ->
    Selector = bson:document(lists:flatten([
        [{'msisdn', {'$exists', true}}],
        [{'msisdn', k_storage_utils:addr_to_doc(Msisdn)} || Msisdn =/= undefined],
        [{'customer_uuid', CustomerUuid} || CustomerUuid =/= undefined],
        [{'customer_uuid', {'$eq', undefined}} || State =:= free],
        [{'customer_uuid', {'$ne', undefined}} || State =:= used]
    ])),
    {ok, Docs} = mongodb_storage:find(static_storage, msisdns, Selector),
    {ok, [doc_to_tuple(Doc) || {_, Doc} <- Docs]}.

-spec get_assigned_to_customer(customer_uuid()) -> {ok, [msisdn()]}.
get_assigned_to_customer(CustomerUuid) ->
    Selector = {
        'customer_uuid', CustomerUuid
    },
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

-spec unassign_from_user(msisdn()) -> ok.
unassign_from_user(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'user_id', undefined
    }},
    ok = mongodb_storage:update(static_storage, msisdns, Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

doc_to_tuple(Doc) ->
    Msisdn = k_storage_utils:doc_to_addr(bsondoc:at(msisdn, Doc)),
    CustomerUuid = bsondoc:at(customer_uuid, Doc),
    UserId = bsondoc:at(user_id, Doc),
    {Msisdn, CustomerUuid, UserId}.
