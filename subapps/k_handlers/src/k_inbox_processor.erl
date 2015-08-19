-module(k_inbox_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#inbox_req_v1{}) ->
    {ok, #inbox_resp_v1{}} | {error, term()}.
process(ReqDTO) ->
    ReqId      = ReqDTO#inbox_req_v1.req_id,
    CustomerUuid = ReqDTO#inbox_req_v1.customer_uuid,
    UserId     = ReqDTO#inbox_req_v1.user_id,
    Operation  = ReqDTO#inbox_req_v1.operation,
    MsgIds     = ReqDTO#inbox_req_v1.msg_ids,
    ?log_debug("CustomerUuid: ~p, UserId: ~p, Operation: ~p, MsgIds: ~p",
        [CustomerUuid, UserId, Operation, MsgIds]),

    case process(CustomerUuid, UserId, Operation, MsgIds) of
        {ok, Result} ->
            {ok, #inbox_resp_v1{
                req_id = ReqId,
                result = Result
            }};
        {error, Reason} ->
            {ok, #inbox_resp_v1{
                req_id = ReqId,
                result = {error, Reason}
            }}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

process(CustomerUuid, UserId, get_info, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            Msgs = [doc2msg(D, false) || {_Id, D} <- Docs],
            New = length([1 || #inbox_msg_info_v1{state = new} <- Msgs]),
            Total = length(Msgs),
            Info = #inbox_info_v1{
                new = New,
                total = Total
            },
            {ok, {info, Info}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, list_all, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            {ok, {messages, [doc2msg(D, false) || {_Id, D} <- Docs]}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, list_new, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId,
        'state'        , bsondoc:atom_to_binary(new)
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            {ok, {messages, [doc2msg(D, false) || {_Id, D} <- Docs]}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, fetch_all, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    Modifier = {
        '$set', {
            'state', bsondoc:atom_to_binary(read)
        }
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            ok = mongodb_storage:update(mailbox_storage, incomings, Selector, Modifier),
            {ok, {messages, [doc2msg(D, true) || {_Id, D} <- Docs]}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, fetch_new, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId,
        'state'        , bsondoc:atom_to_binary(new)
    },
    Modifier = {
        '$set', {
            'state', bsondoc:atom_to_binary(read)
        }
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            ok = mongodb_storage:update(mailbox_storage, incomings, Selector, Modifier),
            {ok, {messages, [doc2msg(D, true) || {_Id, D} <- Docs]}};
        Error ->
            Error
    end;
process(_CustomerUuid, _UserId, fetch_id, MsgIds) ->
    Selector = {
        '_id', {'$in', MsgIds}
    },
    Modifier = {
        '$set', {
            'state', bsondoc:atom_to_binary(read)
        }
    },
    case mongodb_storage:find(mailbox_storage, incomings, Selector) of
        {ok, Docs} ->
            ok = mongodb_storage:update(mailbox_storage, incomings, Selector, Modifier),
            {ok, {messages, [doc2msg(D, true) || {_Id, D} <- Docs]}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, delete_all, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    Command = {
        'count', atom_to_binary(incomings, latin1),
        'query', Selector
    },
    {ok, CountToDelete} =
        case mongodb_storage:command(mailbox_storage, Command) of
            {ok, {missing, true, n, 0.0, ok, 1.0}} ->
                {ok, 0};
            {ok, {n, 0.0, ok, 1.0}} ->
                {ok, 0};
            {ok, {n, Count, ok, 1.0}} ->
                {ok, round(Count)}
        end,
    case mongodb_storage:delete(mailbox_storage, incomings, Selector) of
        ok ->
            {ok, {deleted, CountToDelete}};
        Error ->
            Error
    end;
process(CustomerUuid, UserId, delete_read, undefined) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId,
        'state'        , bsondoc:atom_to_binary(read)
    },
    Command = {
        'count', atom_to_binary(incomings, latin1),
        'query', Selector
    },
    {ok, CountToDelete} =
        case mongodb_storage:command(mailbox_storage, Command) of
            {ok, {missing, true, n, 0.0, ok, 1.0}} ->
                {ok, 0};
            {ok, {n, 0.0, ok, 1.0}} ->
                {ok, 0};
            {ok, {n, Count, ok, 1.0}} ->
                {ok, round(Count)}
        end,
    case mongodb_storage:delete(mailbox_storage, incomings, Selector) of
        ok ->
            {ok, {deleted, CountToDelete}};
        Error ->
            Error
    end;
process(_CustomerUuid, _UserId, delete_id, MsgIds) ->
    Selector = {
        '_id', {'$in', MsgIds}
    },
    case mongodb_storage:delete(mailbox_storage, incomings, Selector) of
        ok ->
            {ok, {deleted, length(MsgIds)}};
        Error ->
            Error
    end;
process(_CustomerUuid, _UserId, mark_as_read_id, MsgIds) ->
    Selector = {
        '_id', {'$in', MsgIds}
    },
    Modifier = {
        '$set', {
            'state', bsondoc:atom_to_binary(read)
        }
    },
    case mongodb_storage:update(mailbox_storage, incomings, Selector, Modifier) of
        ok ->
            {ok, {modified, length(MsgIds)}};
        Error ->
            Error
    end;
process(_CustomerUuid, _UserId, mark_as_unread_id, MsgIds) ->
    Selector = {
        '_id', {'$in', MsgIds}
    },
    Modifier = {
        '$set', {
            'state', bsondoc:atom_to_binary(new)
        }
    },
    case mongodb_storage:update(mailbox_storage, incomings, Selector, Modifier) of
        ok ->
            {ok, {modified, length(MsgIds)}};
        Error ->
            Error
    end;
process(_CustomerUuid, _UserId, _, _) ->
    {ok, {error, not_implemented}}.

doc2msg(Doc, ReturnBody) ->
    #inbox_msg_info_v1{
        msg_id = bsondoc:at('_id', Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at('src_addr', Doc)),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at('dst_addr', Doc)),
        size = size(bsondoc:at('body', Doc)),
        body = if ReturnBody -> bsondoc:at('body', Doc); true -> <<>> end,
        rcv_time = bsondoc:at('rcv_time', Doc),
        state = bsondoc:binary_to_atom(bsondoc:at('state', Doc))
    }.
