-module(k_storage_mailbox).

-include("customer.hrl").
-include("mailbox.hrl").
-include_lib("alley_common/include/logging.hrl").

%% API
-export([
    save/1,
    save_sub/1,
    save_delivery_status/3,

    get_subscription/1,
    get_funnel_subscriptions/0,
    get_subscription_for_oneapi_receipt/1,
    get_subscription_ids/0,
    delete_subscription/1,

    get_items/0,
    get_item/2,

    get_funnel_receipt_ids/2,
    get_oneapi_receipt_ids/2,
    get_incoming_ids/2,

    delete_item/1,

    get_incoming/4,
    get_incomings/7
]).

-type state() :: all | new | read.
-type skip() :: non_neg_integer().
-type limit() :: non_neg_integer().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec save(tuple()) -> ok.
save(#k_mb_oneapi_receipt_sub{} = S) ->
    Selector = {
        '_id', S#k_mb_oneapi_receipt_sub.id
    },
    Modifier = {
        '$set', {
            'customer_uuid'    , S#k_mb_oneapi_receipt_sub.customer_uuid,
            'user_id'          , S#k_mb_oneapi_receipt_sub.user_id,
            'queue_name'       , S#k_mb_oneapi_receipt_sub.queue_name,
            'src_addr'         , k_storage_utils:addr_to_doc(S#k_mb_oneapi_receipt_sub.src_addr),
            'notify_url'       , S#k_mb_oneapi_receipt_sub.notify_url,
            'callback_data'    , S#k_mb_oneapi_receipt_sub.callback_data,
            'req_id'           , S#k_mb_oneapi_receipt_sub.req_id,
            'in_msg_ids'       , S#k_mb_oneapi_receipt_sub.in_msg_ids,
            'created_at'       , S#k_mb_oneapi_receipt_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, oneapi_receipt_subs, Selector, Modifier);
save(#k_mb_incoming{} = I) ->
    Selector = {
        '_id', I#k_mb_incoming.id
    },
    Modifier = {
        '$set', {
            'customer_uuid'   , I#k_mb_incoming.customer_uuid,
            'user_id'         , I#k_mb_incoming.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(I#k_mb_incoming.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(I#k_mb_incoming.dst_addr),
            'rcv_time'        , I#k_mb_incoming.rcv_time,
            'body'            , I#k_mb_incoming.body,
            'encoding'        , bsondoc:atom_to_binary(I#k_mb_incoming.encoding),
            'delivery_attempt', I#k_mb_incoming.delivery_attempt,
            'state'           , bsondoc:atom_to_binary(I#k_mb_incoming.state)
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, incomings, Selector, Modifier);
save(#k_mb_oneapi_receipt{} = R) ->
    Selector = {
        '_id', R#k_mb_oneapi_receipt.id
    },
    Modifier = {
        '$set', {
            'customer_uuid'   , R#k_mb_oneapi_receipt.customer_uuid,
            'user_id'         , R#k_mb_oneapi_receipt.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(R#k_mb_oneapi_receipt.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(R#k_mb_oneapi_receipt.dst_addr),
            'req_id'          , R#k_mb_oneapi_receipt.req_id,
            'in_msg_id'       , R#k_mb_oneapi_receipt.in_msg_id,
            'submit_date'     , R#k_mb_oneapi_receipt.submit_date,
            'done_date'       , R#k_mb_oneapi_receipt.done_date,
            'status'          , bsondoc:atom_to_binary(R#k_mb_oneapi_receipt.status),
            'delivery_attempt', R#k_mb_oneapi_receipt.delivery_attempt
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, oneapi_receipts, Selector, Modifier);
save(#k_mb_funnel_receipt{} = R) ->
    Selector = {
        '_id', R#k_mb_funnel_receipt.id
    },
    Modifier = {
        '$set', {
            'customer_uuid'   , R#k_mb_funnel_receipt.customer_uuid,
            'user_id'         , R#k_mb_funnel_receipt.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.dst_addr),
            'req_id'          , R#k_mb_funnel_receipt.req_id,
            'in_msg_id'       , R#k_mb_funnel_receipt.in_msg_id,
            'submit_date'     , R#k_mb_funnel_receipt.submit_date,
            'done_date'       , R#k_mb_funnel_receipt.done_date,
            'status'          , bsondoc:atom_to_binary(R#k_mb_funnel_receipt.status),
            'delivery_attempt', R#k_mb_funnel_receipt.delivery_attempt
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, funnel_receipts, Selector, Modifier).

-spec save_sub(tuple()) -> ok.
save_sub(#k_mb_oneapi_receipt_sub{} = S) ->
    Selector = {
        '_id', S#k_mb_oneapi_receipt_sub.id
    },
    Modifier = {
        '$set', {
            'type'             , bsondoc:atom_to_binary(k_mb_oneapi_receipt_sub),
            'customer_uuid'    , S#k_mb_oneapi_receipt_sub.customer_uuid,
            'user_id'          , S#k_mb_oneapi_receipt_sub.user_id,
            'queue_name'       , S#k_mb_oneapi_receipt_sub.queue_name,
            'src_addr'         , k_storage_utils:addr_to_doc(S#k_mb_oneapi_receipt_sub.src_addr),
            'notify_url'       , S#k_mb_oneapi_receipt_sub.notify_url,
            'callback_data'    , S#k_mb_oneapi_receipt_sub.callback_data,
            'created_at'       , S#k_mb_oneapi_receipt_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, subscriptions, Selector, Modifier);
save_sub(#k_mb_oneapi_incoming_sub{} = S) ->
    Selector = {
        '_id', S#k_mb_oneapi_incoming_sub.id
    },
    Modifier = {
        '$set', {
            'type'         , bsondoc:atom_to_binary(k_mb_oneapi_incoming_sub),
            'customer_uuid', S#k_mb_oneapi_incoming_sub.customer_uuid,
            'user_id'      , S#k_mb_oneapi_incoming_sub.user_id,
            'priority'     , S#k_mb_oneapi_incoming_sub.priority,
            'queue_name'   , S#k_mb_oneapi_incoming_sub.queue_name,
            'dst_addr'     , k_storage_utils:addr_to_doc(S#k_mb_oneapi_incoming_sub.dst_addr),
            'notify_url'   , S#k_mb_oneapi_incoming_sub.notify_url,
            'criteria'     , S#k_mb_oneapi_incoming_sub.criteria,
            'callback_data', S#k_mb_oneapi_incoming_sub.callback_data,
            'created_at'   , S#k_mb_oneapi_incoming_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, subscriptions, Selector, Modifier);
save_sub(#k_mb_funnel_sub{} = S) ->
    Selector = {
        '_id', S#k_mb_funnel_sub.id
    },
    Modifier = {
        '$set', {
            'type'         , bsondoc:atom_to_binary(k_mb_funnel_sub),
            'customer_uuid', S#k_mb_funnel_sub.customer_uuid,
            'user_id'      , S#k_mb_funnel_sub.user_id,
            'priority'     , S#k_mb_funnel_sub.priority,
            'queue_name'   , S#k_mb_funnel_sub.queue_name,
            'created_at'   , S#k_mb_funnel_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, subscriptions, Selector, Modifier).

-spec save_delivery_status(k_mb_item(), atom(), os:timestamp()) -> ok.
save_delivery_status(#k_mb_funnel_receipt{
    req_id = ReqId,
    in_msg_id = InMsgId
}, Status, Timestamp) ->
    ok = k_dynamic_storage:set_mt_downlink_dlr_status(ReqId, InMsgId, Status, Timestamp);
save_delivery_status(#k_mb_oneapi_receipt{
    req_id = ReqId,
    in_msg_id = InMsgId
}, Status, Timestamp) ->
    ok = k_dynamic_storage:set_mt_downlink_dlr_status(ReqId, InMsgId, Status, Timestamp);
save_delivery_status(#k_mb_incoming{
    id = Id
}, Status, Timestamp) ->
    ok = k_dynamic_storage:set_mo_downlink_dlr_status(Id, Status, Timestamp).

-spec delete_subscription(uuid()) -> ok.
delete_subscription(SubId) ->
    ok = mongodb_storage:delete(mailbox_storage, subscriptions, {'_id', SubId}).

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item = #k_mb_funnel_receipt{}) ->
    Selector = {'_id', Item#k_mb_funnel_receipt.id},
    ok = mongodb_storage:delete(mailbox_storage, funnel_receipts, Selector);
delete_item(Item = #k_mb_oneapi_receipt{}) ->
    Selector = {'_id', Item#k_mb_oneapi_receipt.id},
    ok = mongodb_storage:delete(mailbox_storage, oneapi_receipts, Selector),
    Selector2 = {
        'req_id', Item#k_mb_oneapi_receipt.req_id,
        'in_msg_ids', Item#k_mb_oneapi_receipt.in_msg_id
    },
    Modifier2 = {
        '$pull', {'in_msg_ids', Item#k_mb_oneapi_receipt.in_msg_id}
    },
    ok = mongodb_storage:update(mailbox_storage, oneapi_receipt_subs, Selector2, Modifier2),
    Selector3 = {'in_msg_ids', []},
    ok = mongodb_storage:delete(mailbox_storage, oneapi_receipt_subs, Selector3);
delete_item(Item = #k_mb_incoming{}) ->
    Selector = {'_id', Item#k_mb_incoming.id},
    ok = mongodb_storage:delete(mailbox_storage, incomings, Selector).

-spec get_funnel_receipt_ids(customer_uuid(), user_id()) -> {ok, [{k_mb_funnel_receipt, uuid()}]}.
get_funnel_receipt_ids(CustomerUuid, UserId) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    {ok, Docs} = mongodb_storage:find(mailbox_storage, funnel_receipts, Selector, {'_id', 1}),
    Items = [{k_mb_funnel_receipt, Id} || {Id, _Doc} <- Docs],
    {ok, Items}.

-spec get_oneapi_receipt_ids(customer_uuid(), user_id()) -> {ok, [{k_mb_oneapi_receipt, uuid()}]}.
get_oneapi_receipt_ids(CustomerUuid, UserId) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    {ok, Docs} = mongodb_storage:find(mailbox_storage, funnel_receipts, Selector, {'_id', 1}),
    Items = [{k_mb_funnel_receipt, Id} || {Id, _Doc} <- Docs],
    {ok, Items}.

-spec get_incoming_ids(customer_uuid(), user_id()) ->
    {ok, [{k_mb_incoming, uuid()}]}.
get_incoming_ids(CustomerUuid, UserId) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId
    },
    {ok, Docs} = mongodb_storage:find(mailbox_storage, incomings, Selector, {'_id', 1}),
    Items = [{k_mb_incoming, Id} || {Id, _Doc} <- Docs],
    {ok, Items}.

-spec get_items() -> {ok, [binary()]}.
get_items() ->
    {ok, FunnelReceiptDocs} = mongodb_storage:find(mailbox_storage, funnel_receipts, {}, {'_id', 1}),
    FunnelReceiptIds = [Id || {Id, _} <- FunnelReceiptDocs],
    {ok, OneapiReceiptDocs} = mongodb_storage:find(mailbox_storage, oneapi_receipts, {}, {'_id', 1}),
    ONEAPIReceiptIds = [Id || {Id, _} <- OneapiReceiptDocs],
    {ok, IncomingDocs} = mongodb_storage:find(mailbox_storage, incomings, {}, {'_id', 1}),
    IncomingIds = [Id || {Id, _} <- IncomingDocs],
    {ok, [{k_mb_funnel_receipt, FunnelReceiptIds},
          {k_mb_oneapi_receipt, ONEAPIReceiptIds},
          {k_mb_incoming, IncomingIds}]}.

-spec get_item(ItemType::atom(), ItemId::binary()) -> {ok, Item::tuple()} | no_record.
get_item(k_mb_oneapi_receipt, Id) ->
    case mongodb_storage:find(mailbox_storage, oneapi_receipts, {'_id', Id}) of
        {ok, [{_, Doc}]} ->
            {ok, #k_mb_oneapi_receipt{
                id = Id,
                customer_uuid = bsondoc:at(customer_uuid, Doc),
                user_id = bsondoc:at(user_id, Doc),
                src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
                dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
                req_id = bsondoc:at(req_id, Doc),
                in_msg_id = bsondoc:at(in_msg_id, Doc),
                submit_date = bsondoc:at(submit_date, Doc),
                done_date = bsondoc:at(done_date, Doc),
                status = bsondoc:binary_to_atom(bsondoc:at(status, Doc)),
                delivery_attempt = bsondoc:at(delivery_attempt, Doc)
            }};
        _ ->
            no_record
    end;
get_item(k_mb_funnel_receipt, Id) ->
    case mongodb_storage:find(mailbox_storage, funnel_receipts, {'_id', Id}) of
        {ok, [{_, Doc}]} ->
            {ok, #k_mb_funnel_receipt{
                id = Id,
                customer_uuid = bsondoc:at(customer_uuid, Doc),
                user_id = bsondoc:at(user_id, Doc),
                src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
                dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
                req_id = bsondoc:at(req_id, Doc),
                in_msg_id = bsondoc:at(in_msg_id, Doc),
                submit_date = bsondoc:at(submit_date, Doc),
                done_date = bsondoc:at(done_date, Doc),
                status = bsondoc:binary_to_atom(bsondoc:at(status, Doc)),
                delivery_attempt = bsondoc:at(delivery_attempt, Doc)
            }};
        _ ->
            no_record
    end;
get_item(k_mb_incoming, Id) ->
    case mongodb_storage:find(mailbox_storage, incomings, {'_id', Id}) of
        {ok, [{_, Doc}]} ->
            {ok, doc_to_incoming(Doc)};
        _ ->
            no_record
    end.

-spec get_subscription_for_oneapi_receipt(Receipt::#k_mb_oneapi_receipt{}) ->
    {ok, k_mb_subscription()} | undefined.
get_subscription_for_oneapi_receipt(R = #k_mb_oneapi_receipt{}) ->
    ReqId = R#k_mb_oneapi_receipt.req_id,
    InMsgId = R#k_mb_oneapi_receipt.in_msg_id,
    Selector = {
        'req_id'    , ReqId,
        'in_msg_ids', InMsgId
    },
    case mongodb_storage:find(mailbox_storage, oneapi_receipt_subs, Selector) of
        {ok, []} ->
            CustomerUuid = R#k_mb_oneapi_receipt.customer_uuid,
            UserId = R#k_mb_oneapi_receipt.user_id,
            Selector2 = {
                'customer_uuid', CustomerUuid,
                'user_id'      , UserId,
                'type'         , bsondoc:atom_to_binary(k_mb_oneapi_receipt_sub)
            },
            case mongodb_storage:find(mailbox_storage, subscriptions, Selector2) of
                {ok, []} ->
                    undefined;
                {ok, [{_, SubDoc} | _]} ->
                    Sub = #k_mb_oneapi_receipt_sub{
                        id = bsondoc:at('_id', SubDoc),
                        customer_uuid = bsondoc:at(customer_uuid, SubDoc),
                        user_id = bsondoc:at(user_id, SubDoc),
                        queue_name = bsondoc:at(queue_name, SubDoc),
                        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, SubDoc)),
                        notify_url = bsondoc:at(notify_url, SubDoc),
                        callback_data = bsondoc:at(callback_data, SubDoc),
                        req_id = bsondoc:at(req_id, SubDoc),
                        in_msg_ids = bsondoc:at(in_msg_ids, SubDoc),
                        created_at = bsondoc:at(created_at, SubDoc)
                    },
                    {ok, Sub}
            end;
        {ok, [{_, SubDoc} | _]} ->
            Sub = #k_mb_oneapi_receipt_sub{
                id = bsondoc:at('_id', SubDoc),
                customer_uuid = bsondoc:at(customer_uuid, SubDoc),
                user_id = bsondoc:at(user_id, SubDoc),
                queue_name = bsondoc:at(queue_name, SubDoc),
                src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, SubDoc)),
                notify_url = bsondoc:at(notify_url, SubDoc),
                callback_data = bsondoc:at(callback_data, SubDoc),
                req_id = bsondoc:at(req_id, SubDoc),
                in_msg_ids = bsondoc:at(in_msg_ids, SubDoc),
                created_at = bsondoc:at(created_at, SubDoc)
            },
            {ok, Sub}
    end.

-spec get_subscription(SubId::binary()) ->
    {ok, k_mb_subscription()}.
get_subscription(SubId) ->
    {ok, [{_, Doc}]} = mongodb_storage:find(mailbox_storage, subscriptions, {'_id', SubId}),
    {ok, get_subscription(bsondoc:binary_to_atom(bsondoc:at(type, Doc)), SubId, Doc)}.

get_subscription(k_mb_oneapi_receipt_sub, Id, Doc) ->
    #k_mb_oneapi_receipt_sub{
        id = Id,
        customer_uuid = bsondoc:at(customer_uuid, Doc),
        user_id = bsondoc:at(user_id, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
        notify_url = bsondoc:at(notify_url, Doc),
        callback_data = bsondoc:at(callback_data, Doc),
        created_at = bsondoc:at(created_at, Doc)
    };
get_subscription(k_mb_oneapi_incoming_sub, Id, Doc) ->
    #k_mb_oneapi_incoming_sub{
        id = Id,
        customer_uuid = bsondoc:at(customer_uuid, Doc),
        user_id = bsondoc:at(user_id, Doc),
        priority = bsondoc:at(priority, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
        notify_url = bsondoc:at(notify_url, Doc),
        criteria = bsondoc:at(criteria, Doc),
        callback_data = bsondoc:at(callback_data, Doc),
        created_at = bsondoc:at(created_at, Doc)
    };
get_subscription(k_mb_funnel_sub, Id, Doc) ->
    #k_mb_funnel_sub{
        id = Id,
        customer_uuid = bsondoc:at(customer_uuid, Doc),
        user_id = bsondoc:at(user_id, Doc),
        priority = bsondoc:at(priority, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        created_at = bsondoc:at(created_at, Doc)
    }.

-spec get_funnel_subscriptions() -> {ok, [#k_mb_funnel_sub{}]}.
get_funnel_subscriptions() ->
    Selector = {'type', bsondoc:atom_to_binary(k_mb_funnel_sub)},
    {ok, Docs} =
        mongodb_storage:find(mailbox_storage, subscriptions, Selector),
    Subs = [get_subscription(k_mb_funnel_sub, Id, Doc) || {Id, Doc} <- Docs],
    {ok, Subs}.

-spec get_subscription_ids() -> {ok, [binary()]}.
get_subscription_ids() ->
    {ok, Docs} = mongodb_storage:find(mailbox_storage, subscriptions, {}, {'_id', 1}),
    Ids = [Id || {Id, _} <- Docs],
    {ok, Ids}.

-spec get_incoming(customer_uuid(), user_id(), addr(), limit() | undefined) ->
    {ok, [#k_mb_incoming{}], Total::integer()}.
get_incoming(CustomerUuid, UserId, DstAddr, Limit) ->
    Selector = {
        'customer_uuid', CustomerUuid,
        'user_id'      , UserId,
        'dst_addr'     , k_storage_utils:addr_to_doc(DstAddr)
    },
    {ok, AllDocs} = mongodb_storage:find(mailbox_storage, incomings, Selector),
    Docs = head(Limit, AllDocs),
    Items = [doc_to_incoming(D) || {_, D} <- Docs],
    Pending = length(AllDocs) - length(Docs),
    {ok, Items, Pending}.

-spec get_incomings(timestamp(), timestamp(), customer_uuid(), user_id(), state(), skip(), limit()) ->
    {ok, [#k_mb_incoming{}]} | {error, reason()}.
get_incomings(From, To, CustomerUuid, UserId, State, Skip, Limit) ->
    Selector = bson:document(lists:flatten([
        [{'customer_uuid', CustomerUuid} || CustomerUuid =/= undefined],
        [{'user_id', UserId} || CustomerUuid =/= undefined andalso UserId =/= undefined],
        [{'rcv_time', {'$gte', From, '$lt', To}}],
        [{'state', State} || State =/= all]
    ])),
   case mongodb_storage:find(mailbox_storage, incomings, Selector, {}, Skip, Limit) of
        {ok, Docs} ->
            {ok, [doc_to_incoming(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

head(undefined, List) ->
    head(100, List);
head(N, List) ->
    lists:sublist(List, N).

doc_to_incoming(Doc) ->
    #k_mb_incoming{
        id = bsondoc:at('_id', Doc),
        customer_uuid = bsondoc:at(customer_uuid, Doc),
        user_id = bsondoc:at(user_id, Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
        rcv_time = bsondoc:at(rcv_time, Doc),
        body = bsondoc:at(body, Doc),
        encoding = bsondoc:binary_to_atom(bsondoc:at(encoding, Doc)),
        delivery_attempt = bsondoc:at(delivery_attempt, Doc),
        state = bsondoc:binary_to_atom(bsondoc:at(state, Doc))
    }.
