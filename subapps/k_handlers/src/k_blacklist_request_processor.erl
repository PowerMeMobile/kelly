-module(k_blacklist_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/blacklist_entry.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) -> {ok, record()} | {error, term()}.
process(Req = #k1api_blacklist_request_dto{}) ->
    ReqId       = Req#k1api_blacklist_request_dto.id,
    _CustomerId = Req#k1api_blacklist_request_dto.customer_id,
    _UserId     = Req#k1api_blacklist_request_dto.user_id,
    _Version    = Req#k1api_blacklist_request_dto.version,

    case k_storage_blacklist:get_blacklist_entries() of
        {ok, Entries} ->
            {ok, #k1api_blacklist_response_dto{
                id = ReqId,
                entries = [entry_to_dto(E) || E <- Entries]
            }};
        Error ->
            Error
    end;
process(Req) ->
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

entry_to_dto(Entry) ->
    #blacklist_entry{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    } = Entry,
    #blacklist_entry_dto{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    }.

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
