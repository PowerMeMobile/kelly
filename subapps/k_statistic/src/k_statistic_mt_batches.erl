-module(k_statistic_mt_batches).

-export([
    get_all/1,
    get_details/1,
    get_recipients/2
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
    DealerUuid = ?gv(dealer_uuid, Params),
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),

    CustomerUserDealerSelector =
    if
        CustomerUuid =/= undefined andalso
        UserId =/= undefined ->
            [{'ci', CustomerUuid}, {'ui', UserId}];

        CustomerUuid =/= undefined ->
            [{'ci', CustomerUuid}];

        DealerUuid =/= undefined ->
            {ok, DealerCustomersUuidList} =
                k_storage_customers:get_customers_uuid_by_dealer_uuid(DealerUuid),
            [{'ci', {'$in', DealerCustomersUuidList}}];

        true -> []
    end,
    Selector =
        {'$query',
            bson:document(
                [{'rqt', {'$gte', From, '$lt', To}}] ++
                CustomerUserDealerSelector
            )
        },
    case shifted_storage:find(mt_batches, Selector, {}, Skip, Limit) of
        {ok, Docs} ->
            Batches = [k_storage_utils:doc_to_mt_batch_info(D) || {_, D} <- Docs],
            Uuids = [B#batch_info.customer_uuid || B <- Batches],
            Dict = k_storage_utils:get_uuid_to_customer_dict(Uuids),
            Resp = [build_mt_batch_resp(B, Dict) || B <- Batches],
            {ok, Resp};
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
            Dict = k_storage_utils:get_uuid_to_customer_dict(
                [Batch#batch_info.customer_uuid]),
            Resp = build_mt_batch_resp(Batch, Dict),
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
                    {ok, BlacklistedRecipients} = k_storage_blacklisted_request:get_blacklisted(ReqId),
                    BlacklistedRecipientsNum = length(BlacklistedRecipients),
                    BlacklistedStatus = {blacklisted, BlacklistedRecipientsNum},
                    DoneTime = done_time(Docs),
                    Status = batch_status(Batch#batch_info.status, Statuses),
                    Resp2 = Resp ++ [
                        {statuses, [BlacklistedStatus | Statuses]},
                        {done_time, DoneTime},
                        {status, Status}
                    ],
                    {value, {_, RecipientsNum}, Resp3} = lists:keytake(recipients, 1, Resp2),
                    Resp4 = [{recipients, RecipientsNum + BlacklistedRecipientsNum} | Resp3],
                    {ok, Resp4};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec get_recipients(uuid(), boolean()) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_recipients(ReqId, ShowStatuses) ->
    Selector = {ri, ReqId},

    Projector =
    if
        ShowStatuses ->
            {'_id', 0,
            's'   , 1,
            'da'  , 1};
        true ->
            {'_id', 0,
            'da'  , 1}
    end,

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

is_pending(Statuses) ->
    case proplists:get_value(pending, Statuses) of
        undefined ->
            false;
        _ ->
            true
    end.

done_time(Docs) ->
    StatusTimes = [{S, done_time(S, Doc)} || {_, Doc} <- Docs,
        begin S = binary_to_existing_atom(bsondoc:at(s, Doc), latin1), true end],
    case is_pending(StatusTimes) of
        false ->
            ac_datetime:datetime_to_iso8601(
                ac_datetime:timestamp_to_datetime(
                    lists:max([D || {_S, D} <- StatusTimes])));
        _ ->
            undefined
    end.

batch_status(Blocked, Statuses) ->
    case {Blocked, is_pending(Statuses)} of
        {blocked, true} ->
            blocked;
        {_, true} ->
            processing;
        {_, false} ->
            completed
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

build_mt_batch_resp(Batch, Dict) ->
    CustomerUuid = Batch#batch_info.customer_uuid,
    {CustomerId, CustomerName} =
        case dict:find(CustomerUuid, Dict) of
            {ok, C} ->
                {C#customer.customer_id, C#customer.name};
            error ->
                {undefined, undefined}
        end,
    ReqTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    [
        {req_id, Batch#batch_info.req_id},
        {customer_uuid, CustomerUuid},
        {customer_id, CustomerId},
        {customer_name, CustomerName},
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
        {revenue, Batch#batch_info.price}
    ] ++
    case Batch#batch_info.def_time =/= undefined of
        true ->
            DefTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.def_time),
            DefISO = ac_datetime:datetime_to_iso8601(DefTime),
            [{def_time, DefISO}];
        false ->
            []
    end.

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
    ordsets:to_list(ordsets:from_list(Acc));
build_addrs([D | Ds], Acc) ->
    case bson:at(da, D) of
        <<"xxxxxxxxxx">> ->
            build_addrs(Ds, Acc);
        AddrDoc ->
            Addr = k_storage_utils:doc_to_addr(AddrDoc),
            Addr2 = k_storage_utils:addr_to_proplist(Addr),
            AddrWithStatus = add_status_if_defined(D, Addr2),
            build_addrs(Ds, [AddrWithStatus | Acc])
    end.

add_status_if_defined(Doc, Proplist) ->
    case bson:at(s, Doc) of
        null ->
            Proplist;
        Status ->
            [{status, Status} | Proplist]
    end.
