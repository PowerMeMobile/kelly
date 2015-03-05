-module(k_statistic_mt_batches).

-export([
    get_all/1,
    get_one/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_all([{atom(), term()}]) -> [[{atom(), term()}]].
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
    {ok, Docs} = shifted_storage:find(mt_batches, Selector, {}, Skip, Limit),
    [build_mt_batch_simple_response(Batch) || {_, Doc} <- Docs,
        begin Batch = k_storage_utils:doc_to_mt_batch_info(Doc), true end].

-spec get_one(uuid()) -> [[{atom(), term()}]].
get_one(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {},
    {ok, Doc} = shifted_storage:find_one(mt_batches, Selector, Projector),
    Batch = k_storage_utils:doc_to_mt_batch_info(Doc),
    Resp = build_mt_batch_simple_response(Batch),
    Selector2 = {'ri', ReqId},
    Projector2 = {
        '_id', 0,
        s    , 1,
        rpt  , 1,
        dt   , 1
    },
    {ok, Docs} = shifted_storage:find(mt_messages, Selector2, Projector2),
    Statuses = build_statuses(Docs, Batch#batch_info.messages),
    DoneTime = get_done_time(Docs),
    Resp ++ [{statuses, Statuses}, {done_time, DoneTime}].

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

build_mt_batch_simple_response(Batch) ->
    ReqTime  = ac_datetime:timestamp_to_datetime(Batch#batch_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    [
        {req_id, Batch#batch_info.req_id},
        {customer_uuid, Batch#batch_info.customer_id},
        {user_id, Batch#batch_info.user_id},
        {client_type, Batch#batch_info.client_type},
        {src_addr, addr_to_proplist(Batch#batch_info.src_addr)},
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

addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi}
    ];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi},
        {ref_num, RefNum}
    ].
