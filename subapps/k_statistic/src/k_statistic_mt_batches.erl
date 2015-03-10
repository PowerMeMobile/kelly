-module(k_statistic_mt_batches).

-export([
    get_all/1,
    get_details/1,
    get_recipients/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all([{atom(), term()}]) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_all(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),
    CustomerUuidSel =
        case ?gv(customer_uuid, Params) of
            undefined -> [];
            CustomerId -> [{'ci', CustomerId}]
        end,
    UserIdSel =
        case ?gv(user_id, Params) of
            undefined -> [];
            UserId -> [{'ui', UserId}]
        end,
    Selector =
        {'$query',
            bson:document(
                [{'rqt', {'$gte', From, '$lt', To}}] ++
                CustomerUuidSel ++ UserIdSel
            )
        },
    case shifted_storage:find(mt_batches, Selector, {}, Skip, Limit) of
        {ok, Docs} ->
            {ok, [build_mt_batch_response(Batch)
                    || {_, Doc} <- Docs,
                    begin
                        Batch = k_storage_utils:doc_to_mt_batch_info(Doc), true
                    end]};
        {error, Error} ->
            {error, Error}
    end.

-spec get_details(uuid()) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_details(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {},
    case shifted_storage:find_one(mt_batches, Selector, Projector) of
        {ok, Doc} ->
            Batch = k_storage_utils:doc_to_mt_batch_info(Doc),
            Resp = build_mt_batch_response(Batch),
            Selector2 = {'ri', ReqId},
            Projector2 = {
                '_id', 0,
                s    , 1,
                rpt  , 1,
                dt   , 1
            },
            case shifted_storage:find(mt_messages, Selector2, Projector2) of
                {ok, Docs} ->
                    Statuses = build_statuses(Docs, Batch#batch_info.messages),
                    DoneTime = get_done_time(Docs),
                    {ok, Resp ++ [{statuses, Statuses}, {done_time, DoneTime}]};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec get_recipients(uuid()) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_recipients(ReqId) ->
    Selector = {ri, ReqId},
    Projector = {
        '_id', 0,
        da   , 1
    },
    case shifted_storage:find(mt_messages, Selector, Projector) of
        {ok, Docs} ->
            {ok, build_addrs([D || {_, D} <- Docs])};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

build_statuses(Docs, TotalMsgs) ->
    Statuses = merge([
        {binary_to_existing_atom(bsondoc:at(s, Doc), latin1), 1}
        || {_, Doc} <- Docs
    ]),
    Msgs = lists:foldl(fun({_S, N}, Acc) -> N + Acc end, 0, Statuses),
    Statuses2 =
        case Msgs < TotalMsgs of
            true ->
                Pending = proplists:get_value(pending, Statuses),
                Pending2 = Pending + TotalMsgs - Msgs,
                [{pending, Pending2} | proplists:delete(pending, Statuses)];
            false ->
                Statuses
        end,
    [{K,V} || {K,V} <- Statuses2, V =/= 0].

get_done_time(Docs) ->
    StatusTimes = [{S, done_time(S, Doc)} || {_, Doc} <-Docs,
        begin S = binary_to_existing_atom(bsondoc:at(s, Doc), latin1), true end],
    case proplists:get_value(pending, StatusTimes) of
        undefined ->
            ac_datetime:datetime_to_iso8601(
                ac_datetime:timestamp_to_datetime(
                    lists:max([D || {_S, D} <- StatusTimes])));
        _ ->
            undefined
    end.

merge(Pairs) ->
    Dict = dict:from_list([
        % yet not done time
        {pending, 0},

        % done time
        {submitted, 0},
        {failed, 0},
        {blocked, 0},
        {enroute, 0},
        {delivered, 0},
        {expired, 0},
        {deleted, 0},
        {undeliverable, 0},
        {accepted, 0},
        {unknown, 0},
        {rejected, 0},
        {unrecognized, 0}
    ]),
    dict:to_list(merge(Pairs, Dict)).

merge([], Dict) ->
    Dict;
merge([{Key, Value}|Pairs], Dict) ->
    NewDict = dict:update_counter(Key, Value, Dict),
    merge(Pairs, NewDict).

build_mt_batch_response(Batch) ->
    %% TODO: add def_time if defined
    ReqTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    [
        {req_id, Batch#batch_info.req_id},
        {customer_uuid, Batch#batch_info.customer_id},
        {user_id, Batch#batch_info.user_id},
        {client_type, Batch#batch_info.client_type},
        {req_type, Batch#batch_info.req_type},
        {src_addr, k_storage_utils:addr_to_proplist(Batch#batch_info.src_addr)},
        {encoding, Batch#batch_info.encoding},
        {body, Batch#batch_info.body},
        {reg_dlr, Batch#batch_info.reg_dlr},
        {req_time, ReqISO},
        {recipients, Batch#batch_info.recipients},
        {messages, Batch#batch_info.messages},
        {price, Batch#batch_info.price}
    ].

done_time(pending, Doc) ->
    bson:at(rqt, Doc);
done_time(submitted, Doc) ->
    bson:at(rpt, Doc);
done_time(failed, Doc) ->
    bson:at(rpt, Doc);
done_time(blocked, Doc) ->
    bson:at(rpt, Doc);
done_time(_, Doc) ->
    bson:at(dt, Doc).

build_addrs(Docs) ->
    build_addrs(Docs, []).

build_addrs([], Acc) ->
    lists:sort(sets:to_list(sets:from_list(Acc)));
build_addrs([D | Ds], Acc) ->
    case bson:at(da, D) of
        <<"xxxxxxxxxx">> ->
            build_addrs(Ds, Acc);
        AddrDoc ->
            Addr = k_storage_utils:doc_to_addr(AddrDoc),
            Addr2 = k_storage_utils:addr_to_proplist(Addr),
            build_addrs(Ds, [Addr2 | Acc])
    end.
