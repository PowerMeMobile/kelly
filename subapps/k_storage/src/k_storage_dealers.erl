-module(k_storage_dealers).

%% API
-export([
    set_dealer/2,
    get_dealers/0,
    get_dealer_by_uuid/1,
    del_dealer/1
]).

-include("storages.hrl").
-include("dealer.hrl").

-define(DEALERS_COLLECTION, dealers).

%% ===================================================================
%% API
%% ===================================================================

-spec set_dealer(dealer_uuid(), #dealer_v1{}) -> ok | {error, term()}.
set_dealer(DealerUUID, Dealer = #dealer_v1{}) ->

    Update = {
        '$set', {
            'name'               , Dealer#dealer_v1.name,
            'network_map_id'     , Dealer#dealer_v1.network_map_id,
            'default_provider_id', Dealer#dealer_v1.default_provider_id,
            'interfaces',
                [bsondoc:atom_to_binary(I) || I <- Dealer#dealer_v1.interfaces],
            'features'           , features_to_docs(Dealer#dealer_v1.features),
            'state'              , bsondoc:atom_to_binary(Dealer#dealer_v1.state)
        }
    },
    Query = {
        '_id', DealerUUID,
        'state', {'$ne', bsondoc:atom_to_binary(?DELETED_ST)}
    },
    FindAndModify = {
        'findandmodify', atom_to_binary(?DEALERS_COLLECTION, utf8),
        'query', Query,
        'update', Update,
        'new', 'false',
        'fields', {'state', 1},
        'upsert', 'true'
    },
    case mongodb_storage:command(static_storage, FindAndModify) of
        {ok, Result} ->
            case bsondoc:at(value, Result) of
                undefined ->
                    %% new dealer, nothing
                    ok;
                Value ->
                    OldState = bsondoc:binary_to_atom(bsondoc:at(state, Value)),
                    NewState = Dealer#dealer_v1.state,
                    update_customers_state(DealerUUID, OldState, NewState)
            end;

        {error, Reason} ->
            {error, Reason}
    end.


update_customers_state(_DealerUUID, State, State) ->
    %% state did not change, skip
    ok;
update_customers_state(DealerUUID, _OldState, ?DELETED_ST) ->
    ok = k_storage_customers:delete_dealer_customers(DealerUUID);
update_customers_state(DealerUUID, _OldState, ?DEACTIVATED_ST) ->
    ok = k_storage_customers:deactivate_dealer_customers(DealerUUID);
update_customers_state(DealerUUID, ?ACTIVE_ST, ?BLOCKED_ST) ->
    ok = k_storage_customers:block_dealer_customers(DealerUUID);
update_customers_state(DealerUUID, _OldState, ?ACTIVE_ST) ->
    ok = k_storage_customers:unblock_dealer_customers(DealerUUID).


-spec get_dealers() -> {ok, [dealer()]} | {error, term()}.
get_dealers() ->
    Selector = {
        state, {'$ne', bsondoc:atom_to_binary(?DELETED_ST)}
    },
    case mongodb_storage:find(static_storage, ?DEALERS_COLLECTION, Selector) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_dealer_by_uuid(dealer_uuid()) ->
    {ok, dealer()} | {error, no_entry} | {error, term()}.
get_dealer_by_uuid(DealerUUID) when is_binary(DealerUUID) ->
    Selector = {
        '_id', DealerUUID,
        state, {'$ne', bsondoc:atom_to_binary(?DELETED_ST)}
    },
    case mongodb_storage:find_one(static_storage, ?DEALERS_COLLECTION, Selector) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        {error, Reason} ->
            {error, Reason}
    end.


-spec del_dealer(dealer_uuid()) -> ok | {error, no_entry} | {error, term()}.
del_dealer(DealerUUID) when is_binary(DealerUUID) ->
    case get_dealer_by_uuid(DealerUUID) of
        {ok, #dealer_v1{} = Dealer} ->
            DeletedDealer = Dealer#dealer_v1{state = ?DELETED_ST},
            case set_dealer(DealerUUID, DeletedDealer) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, no_entry} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

doc_to_record(Doc) ->
    DealerUUID = bsondoc:at('_id', Doc),
    Name = bsondoc:at(name, Doc),
    NetMapId = bsondoc:at(network_map_id, Doc),
    DefProvId = bsondoc:at(default_provider_id, Doc),
    Interfaces =
        [bsondoc:binary_to_atom(I) || I <- bsondoc:at(interfaces, Doc, [])],
    Features = docs_to_features(bsondoc:at(features, Doc, [])),
    State = bsondoc:binary_to_atom(bsondoc:at(state, Doc)),

    #dealer_v1{
        id = DealerUUID,
        name = Name,
        network_map_id = NetMapId,
        default_provider_id = check_undefined(DefProvId),
        features = Features,
        interfaces = Interfaces,
        state = State
    }.


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
