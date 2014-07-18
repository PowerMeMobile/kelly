-module(k_inbox_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_process_inbox_request_dto{}) ->
    {ok, #k1api_process_inbox_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId      = ReqDTO#k1api_process_inbox_request_dto.id,
    CustomerId = ReqDTO#k1api_process_inbox_request_dto.customer_id,
    UserId     = ReqDTO#k1api_process_inbox_request_dto.user_id,
    Operation  = ReqDTO#k1api_process_inbox_request_dto.operation,
    MessageIds = ReqDTO#k1api_process_inbox_request_dto.message_ids,
    ?log_debug("CID: ~p, UID: ~p, Op: ~p, MIDs: ~p",
        [CustomerId, UserId, Operation, MessageIds]),

    case process(CustomerId, UserId, Operation, MessageIds) of
        {ok, Result} ->
            {ok, #k1api_process_inbox_response_dto{
                id = ReqId,
                result = Result
            }};
        {error, Reason} ->
            {ok, #k1api_process_inbox_response_dto{
                id = ReqId,
                result = {error, Reason}
            }}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

process(CustomerId, UserId, list_all, _) ->
    Selector = {
        'customer_id', CustomerId,
        'user_id'    , UserId
    },
    case mongodb_storage:find(static_storage, mb_incoming_sms, Selector) of
        {ok, []} ->
            Selector2 = {
                'customer_id', CustomerId
            },
            case mongodb_storage:find(static_storage, mb_incoming_sms, Selector2) of
                {ok, Docs} ->
                    {ok, {messages, [doc2msg(D) || {_Id, D} <- Docs]}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
process(CustomerId, UserId, list_new, _) ->
    {ok, {}};
process(CustomerId, UserId, fetch_all, _) ->
    {ok, {}};
process(CustomerId, UserId, fetch_new, _) ->
    {ok, {}};
process(CustomerId, UserId, fetch_id, _MessageIds) ->
    {ok, {}};
process(CustomerId, UserId, kill_all, _) ->
    {ok, {}};
process(CustomerId, UserId, kill_old, _) ->
    {ok, {}};
process(CustomerId, UserId, kill_id, _MessageIds) ->
    {ok, {}};
process(CustomerId, UserId, _, _) ->
    {ok, {}}.

doc2msg(Doc) ->
    #k1api_process_inbox_response_message_dto{
        id = bsondoc:at('_id', Doc),
        new = true,
        from = k_storage_utils:doc_to_addr(bsondoc:at('source_addr', Doc)),
        to = k_storage_utils:doc_to_addr(bsondoc:at('dest_addr', Doc)),
        timestamp = bsondoc:at('received', Doc),
        size = size(bsondoc:at('message_body', Doc)),
        text = bsondoc:at('message_body', Doc)
    }.
