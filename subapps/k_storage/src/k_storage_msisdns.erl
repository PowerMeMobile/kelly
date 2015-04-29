-module(k_storage_msisdns).

-export([
    create/1,
    set_customer_uuid/2,
    delete/1,
    get_one/1,
    get_many/3
]).

-include("storages.hrl").
-include("customer.hrl").

-type state() :: all | free | used.

%% ===================================================================
%% API
%% ===================================================================

-spec get_one(addr()) -> {ok, {addr(), customer_uuid()}} | {error, not_found}.
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

-spec get_many(addr(), customer_uuid(), state()) -> {ok, [{addr(), customer_uuid()}]}.
get_many(Msisdn, CustomerUuid, State) ->
    Selector = bson:document(lists:flatten([
        [{'msisdn', k_storage_utils:addr_to_doc(Msisdn)} || Msisdn =/= undefined],
        [{'customer_uuid', CustomerUuid} || CustomerUuid =/= undefined],
        [{'customer_uuid', {'$eq', undefined}} || State =:= free],
        [{'customer_uuid', {'$ne', undefined}} || State =:= used]
    ])),
    {ok, Docs} = mongodb_storage:find(static_storage, msisdns, Selector),
    {ok, [doc_to_tuple(Doc) || {_, Doc} <- Docs]}.

-spec delete(addr()) -> ok.
delete(Msisdn) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    ok = mongodb_storage:delete(static_storage, msisdns, Selector).

-spec create(addr()) -> ok | {error, already_used}.
create(Msisdn) ->
    Modifier = {
        'msisdn'       , k_storage_utils:addr_to_doc(Msisdn),
        'customer_uuid', undefined,
        'user_id'      , undefined
    },
    case get_one(Msisdn) of
        {error, not_found} ->
            {ok, _ID} = mongodb_storage:insert(static_storage, msisdns, Modifier),
            ok;
        {ok, _} ->
            {error, already_used}
    end.

-spec set_customer_uuid(addr(), customer_uuid()) -> ok | {error, not_found}.
set_customer_uuid(Msisdn, CustomerUuid) ->
    Selector = {
        'msisdn', k_storage_utils:addr_to_doc(Msisdn)
    },
    Modifier = {'$set', {
        'customer_uuid', CustomerUuid
    }},
    case get_one(Msisdn) of
        {error, not_found} ->
            {error, not_found};
        {ok, _} ->
            ok = mongodb_storage:upsert(static_storage, msisdns, Selector, Modifier)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

doc_to_tuple(Doc) ->
    Msisdn = k_storage_utils:doc_to_addr(bsondoc:at(msisdn, Doc)),
    CustomerUuid = bsondoc:at(customer_uuid, Doc),
    {Msisdn, CustomerUuid}.
