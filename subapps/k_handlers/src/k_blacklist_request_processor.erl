-module(k_blacklist_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/blacklist_entry.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_blacklist_request_dto{}) ->
    {ok, #k1api_blacklist_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId       = ReqDTO#k1api_blacklist_request_dto.id,
    _CustomerId = ReqDTO#k1api_blacklist_request_dto.customer_id,
    _UserId     = ReqDTO#k1api_blacklist_request_dto.user_id,
    _Version    = ReqDTO#k1api_blacklist_request_dto.version,

    case k_storage_blacklist:get_blacklist_entries() of
        {ok, Entries} ->
            {ok, #k1api_blacklist_response_dto{
                id = ReqId,
                entries = [entry_to_dto(E) || E <- Entries]
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
