-module(k_blacklist_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/blacklist_entry.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #blacklist_req_v1{}) ->
    ReqId = Req#blacklist_req_v1.req_id,

    case k_storage_blacklist:get_blacklist_entries() of
        {ok, Entries} ->
            {ok, #blacklist_resp_v1{
                req_id = ReqId,
                entries = [entry_to_v1(E) || E <- Entries]
            }};
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================

entry_to_v1(Entry) ->
    #blacklist_entry{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    } = Entry,
    #blacklist_entry_v1{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    }.
