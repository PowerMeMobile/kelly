-module(k_storage_blacklisted_request).

%% API
-export([
    set_blacklisted/2,
    get_blacklisted/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

-define(COLLECTION_NAME, mt_blacklisted).

%% ===================================================================
%% API
%% ===================================================================

-spec set_blacklisted(ReqId :: uuid(), [addr()]) -> ok | {error, term()}.
set_blacklisted(_ReqId, []) -> ok;
set_blacklisted(ReqId, BlacklistedRecipients) ->
    Selector = {'_id', ReqId},
    Modifier = {'$setOnInsert', {
        'numbers', addr_to_doc(BlacklistedRecipients)
    }},
    mongodb_storage:upsert(curr_dynamic_storage, ?COLLECTION_NAME, Selector, Modifier).


-spec get_blacklisted(ReqId :: uuid()) -> {ok, [addr()]} | {error, term()}.
get_blacklisted(ReqId) ->
    Selector = {'_id', ReqId},
    case shifted_storage:find_one(?COLLECTION_NAME, Selector, {}) of
        {ok, Doc} ->
            Numbers = bsondoc:at('numbers', Doc, []),
            {ok, doc_to_addr(Numbers)};
        {error, no_entry} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

addr_to_doc(AddrList) ->
    addr_to_doc(AddrList, _Acc = []).
addr_to_doc([], Acc) -> Acc;
addr_to_doc([Addr = #addr{} | RestAddrList], Acc) ->

    Doc = {
        'addr', Addr#addr.addr,
        'ton' , Addr#addr.ton,
        'npi' , Addr#addr.npi
    },

    addr_to_doc(RestAddrList, [Doc | Acc]).

doc_to_addr(Addr) ->
    doc_to_addr(Addr, _Acc = []).
doc_to_addr([], Acc) -> Acc;
doc_to_addr([Doc | RestDocs], Acc) ->
    Addr = #addr{
        addr = bson:at('addr', Doc),
        ton = bson:at('ton', Doc),
        npi = bson:at('npi', Doc)
    },
    doc_to_addr(RestDocs, [Addr | Acc]).
