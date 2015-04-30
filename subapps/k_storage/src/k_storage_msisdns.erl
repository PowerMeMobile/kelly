-module(k_storage_msisdns).

-export([
    create/1,
    delete/1,
    get_one/1,
    get_many/3,

    assign/2,
    unassign/1,
    get_assigned_to/1
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

-spec get_assigned_to(customer_uuid()) -> {ok, [msisdn()]}.
get_assigned_to(CustomerUuid) ->
    Selector = {
        'customer_uuid', CustomerUuid
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

-spec assign(msisdn(), customer_uuid()) -> ok.
assign(Msisdn, CustomerUuid) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', CustomerUuid,
        'user_id'      , undefined
    }},
    ok = mongodb_storage:upsert(static_storage, msisdns, Selector, Modifier).

-spec unassign(msisdn()) -> ok.
unassign(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', undefined,
        'user_id'      , undefined
    }},
    ok = mongodb_storage:upsert(static_storage, msisdns, Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

doc_to_tuple(Doc) ->
    Msisdn = k_storage_utils:doc_to_addr(bsondoc:at(msisdn, Doc)),
    CustomerUuid = bsondoc:at(customer_uuid, Doc),
    {Msisdn, CustomerUuid}.
