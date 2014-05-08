-module(k_storage_blacklist).

%% API
-export([
    set_blacklist_entry/2,
    get_blacklist_entry/1,
    get_blacklist_entries/0,
    del_blacklist_entry/1
]).

-include("storages.hrl").
-include("blacklist_entry.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_blacklist_entry(blacklist_entry_id(), #blacklist_entry{}) -> ok | {error, term()}.
set_blacklist_entry(EntryId, Entry)->
    Modifier = {
        '$set', {
            'dst_addr', addr_to_doc(Entry#blacklist_entry.dst_addr),
            'src_addr', addr_to_doc(Entry#blacklist_entry.src_addr)
        }
    },
    mongodb_storage:upsert(static_storage, blacklist, {'_id', EntryId}, Modifier).

-spec get_blacklist_entry(blacklist_entry_id()) -> {ok, #blacklist_entry{}} | {error, no_entry} | {error, term()}.
get_blacklist_entry(EntryId) ->
    case mongodb_storage:find_one(static_storage, blacklist, {'_id', EntryId}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec get_blacklist_entries() -> {ok, [#blacklist_entry{}]} | {error, term()}.
get_blacklist_entries() ->
    case mongodb_storage:find(static_storage, blacklist, {}) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        Error ->
            Error
    end.

-spec del_blacklist_entry(blacklist_entry_id()) -> ok | {error, no_entry} | {error, term()}.
del_blacklist_entry(EntryId) ->
    mongodb_storage:delete(static_storage, blacklist, {'_id', EntryId}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    Id = bsondoc:at('_id', Doc),
    DstAddr = doc_to_addr(bsondoc:at(dst_addr, Doc)),
    SrcAddr = doc_to_addr(bsondoc:at(src_addr, Doc)),
    #blacklist_entry{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    }.

doc_to_addr(undefined) ->
    undefined;
doc_to_addr(Doc) ->
    #addr{
        addr = bsondoc:at('addr', Doc),
        ton = bsondoc:at('ton', Doc),
        npi = bsondoc:at('npi', Doc)
    }.

addr_to_doc(undefined) ->
    undefined;
addr_to_doc(#addr{addr = Addr, ton = Ton, npi = Npi}) ->
    {
        'addr', Addr,
        'ton' , Ton,
        'npi' , Npi
    }.
