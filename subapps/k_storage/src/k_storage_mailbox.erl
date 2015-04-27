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
    get_funnel_receipts/2,
    delete_item/1,

    set_pending/4,
    get_pending/2,

    get_incoming_sms/4
]).

%% ===================================================================
%% API
%% ===================================================================

-spec save(tuple()) -> ok.
save(#k_mb_oneapi_receipt_sub{} = Sub) ->
    Selector = {
        '_id', Sub#k_mb_oneapi_receipt_sub.id
    },
    Modifier = {
        '$set', {
            'customer_id'      , Sub#k_mb_oneapi_receipt_sub.customer_id,
            'user_id'          , Sub#k_mb_oneapi_receipt_sub.user_id,
            'queue_name'       , Sub#k_mb_oneapi_receipt_sub.queue_name,
            'src_addr'         , k_storage_utils:addr_to_doc(Sub#k_mb_oneapi_receipt_sub.src_addr),
            'notify_url'       , Sub#k_mb_oneapi_receipt_sub.notify_url,
            'callback_data'    , Sub#k_mb_oneapi_receipt_sub.callback_data,
            'req_id'           , Sub#k_mb_oneapi_receipt_sub.req_id,
            'in_msg_ids'       , Sub#k_mb_oneapi_receipt_sub.in_msg_ids,
            'created_at'       , Sub#k_mb_oneapi_receipt_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, oneapi_receipt_subs, Selector, Modifier);
save(#k_mb_incoming_sms{} = Sms) ->
    Selector = {
        '_id', Sms#k_mb_incoming_sms.id
    },
    Modifier = {
        '$set', {
            'customer_id'     , Sms#k_mb_incoming_sms.customer_id,
            'user_id'         , Sms#k_mb_incoming_sms.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(Sms#k_mb_incoming_sms.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(Sms#k_mb_incoming_sms.dst_addr),
            'received'        , Sms#k_mb_incoming_sms.received,
            'body'            , Sms#k_mb_incoming_sms.body,
            'encoding'        , bsondoc:atom_to_binary(Sms#k_mb_incoming_sms.encoding),
            'delivery_attempt', Sms#k_mb_incoming_sms.delivery_attempt,
            'created_at'      , Sms#k_mb_incoming_sms.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, incoming_sms, Selector, Modifier);
save(#k_mb_oneapi_receipt{} = R) ->
    Selector = {
        '_id', R#k_mb_oneapi_receipt.id
    },
    Modifier = {
        '$set', {
            'customer_id'     , R#k_mb_oneapi_receipt.customer_id,
            'user_id'         , R#k_mb_oneapi_receipt.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(R#k_mb_oneapi_receipt.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(R#k_mb_oneapi_receipt.dst_addr),
            'req_id'          , R#k_mb_oneapi_receipt.req_id,
            'in_msg_id'       , R#k_mb_oneapi_receipt.in_msg_id,
            'submit_date'     , R#k_mb_oneapi_receipt.submit_date,
            'done_date'       , R#k_mb_oneapi_receipt.done_date,
            'status'          , bsondoc:atom_to_binary(R#k_mb_oneapi_receipt.status),
            'delivery_attempt', R#k_mb_oneapi_receipt.delivery_attempt,
            'created_at'      , R#k_mb_oneapi_receipt.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, oneapi_receipts, Selector, Modifier);
save(#k_mb_funnel_receipt{} = R) ->
    Selector = {
        '_id', R#k_mb_funnel_receipt.id
    },
    Modifier = {
        '$set', {
            'customer_id'     , R#k_mb_funnel_receipt.customer_id,
            'user_id'         , R#k_mb_funnel_receipt.user_id,
            'src_addr'        , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.src_addr),
            'dst_addr'        , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.dst_addr),
            'req_id'          , R#k_mb_funnel_receipt.req_id,
            'in_msg_id'       , R#k_mb_funnel_receipt.in_msg_id,
            'submit_date'     , R#k_mb_funnel_receipt.submit_date,
            'done_date'       , R#k_mb_funnel_receipt.done_date,
            'status'          , bsondoc:atom_to_binary(R#k_mb_funnel_receipt.status),
            'delivery_attempt', R#k_mb_funnel_receipt.delivery_attempt,
            'created_at'      , R#k_mb_funnel_receipt.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, funnel_receipts, Selector, Modifier).

-spec save_sub(tuple()) -> ok.
save_sub(#k_mb_oneapi_receipt_sub{} = Sub) ->
    Selector = {
        '_id', Sub#k_mb_oneapi_receipt_sub.id
    },
    Modifier = {
        '$set', {
            'type'             , bsondoc:atom_to_binary(k_mb_oneapi_receipt_sub),
            'customer_id'      , Sub#k_mb_oneapi_receipt_sub.customer_id,
            'user_id'          , Sub#k_mb_oneapi_receipt_sub.user_id,
            'queue_name'       , Sub#k_mb_oneapi_receipt_sub.queue_name,
            'src_addr'         , k_storage_utils:addr_to_doc(Sub#k_mb_oneapi_receipt_sub.src_addr),
            'notify_url'       , Sub#k_mb_oneapi_receipt_sub.notify_url,
            'callback_data'    , Sub#k_mb_oneapi_receipt_sub.callback_data,
            'created_at'       , Sub#k_mb_oneapi_receipt_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, subscriptions, Selector, Modifier);
save_sub(#k_mb_oneapi_incoming_sms_sub{} = Sub) ->
    Selector = {
        '_id', Sub#k_mb_oneapi_incoming_sms_sub.id
    },
    Modifier = {
        '$set', {
            'type'         , bsondoc:atom_to_binary(k_mb_oneapi_incoming_sms_sub),
            'customer_id'  , Sub#k_mb_oneapi_incoming_sms_sub.customer_id,
            'user_id'      , Sub#k_mb_oneapi_incoming_sms_sub.user_id,
            'priority'     , Sub#k_mb_oneapi_incoming_sms_sub.priority,
            'queue_name'   , Sub#k_mb_oneapi_incoming_sms_sub.queue_name,
            'dst_addr'     , k_storage_utils:addr_to_doc(Sub#k_mb_oneapi_incoming_sms_sub.dst_addr),
            'notify_url'   , Sub#k_mb_oneapi_incoming_sms_sub.notify_url,
            'criteria'     , Sub#k_mb_oneapi_incoming_sms_sub.criteria,
            'callback_data', Sub#k_mb_oneapi_incoming_sms_sub.callback_data,
            'created_at'   , Sub#k_mb_oneapi_incoming_sms_sub.created_at
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, subscriptions, Selector, Modifier);
save_sub(#k_mb_funnel_sub{} = Sub) ->
    Selector = {
        '_id', Sub#k_mb_funnel_sub.id
    },
    Modifier = {
        '$set', {
            'type'       , bsondoc:atom_to_binary(k_mb_funnel_sub),
            'customer_id', Sub#k_mb_funnel_sub.customer_id,
            'user_id'    , Sub#k_mb_funnel_sub.user_id,
            'priority'   , Sub#k_mb_funnel_sub.priority,
            'queue_name' , Sub#k_mb_funnel_sub.queue_name,
            'created_at' , Sub#k_mb_funnel_sub.created_at
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
save_delivery_status(#k_mb_incoming_sms{
    id = Id
}, Status, Timestamp) ->
    ok = k_dynamic_storage:set_mo_downlink_dlr_status(Id, Status, Timestamp).

-spec delete_subscription(SubscriptionID::binary()) -> ok.
delete_subscription(SubscriptionID) ->
    ok = mongodb_storage:delete(mailbox_storage, subscriptions, {'_id' , SubscriptionID}).

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item = #k_mb_funnel_receipt{}) ->
    Selector = {'_id', Item#k_mb_funnel_receipt.id},
    ok = mongodb_storage:delete(mailbox_storage, funnel_receipts, Selector);
delete_item(Item = #k_mb_oneapi_receipt{}) ->
    Selector = {'_id', Item#k_mb_oneapi_receipt.id},
    ok = mongodb_storage:delete(mailbox_storage, oneapi_receipts, Selector),
    ok = mongodb_storage:delete(mailbox_storage, pending_items, Selector),
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
delete_item(Item = #k_mb_incoming_sms{}) ->
    Selector = {'_id', Item#k_mb_incoming_sms.id},
    ok = mongodb_storage:delete(mailbox_storage, incoming_sms, Selector),
    ok = mongodb_storage:delete(mailbox_storage, pending_items, Selector).

-spec get_funnel_receipts(binary(), binary()) -> {ok, [{k_mb_funnel_receipt, ID::binary()}]}.
get_funnel_receipts(CustomerID, UserID) ->
    Selector = {
        customer_id, CustomerID,
        user_id, UserID
    },
    {ok, FunnelReceiptDocs} = mongodb_storage:find(mailbox_storage, funnel_receipts, Selector, {'_id' , 1}),
    FunnelReceipts = [{k_mb_funnel_receipt, RID} || {RID, _} <- FunnelReceiptDocs],
    {ok, FunnelReceipts}.

-spec get_items() -> {ok, [binary()]}.
get_items() ->
    {ok, FunnelReceiptDocs} = mongodb_storage:find(mailbox_storage, funnel_receipts, {}, {'_id' , 1}),
    FunnelReceiptIds = [RID || {RID, _} <- FunnelReceiptDocs],
    {ok, OneapiReceiptDocs} = mongodb_storage:find(mailbox_storage, oneapi_receipts, {}, {'_id' , 1}),
    ONEAPIReceiptIds = [RID || {RID, _} <- OneapiReceiptDocs],
    {ok, IncomingSmsDocs} = mongodb_storage:find(mailbox_storage, incoming_sms, {}, {'_id' , 1}),
    IncomingSmsIds = [ISID || {ISID, _} <- IncomingSmsDocs],
    {ok, [{k_mb_funnel_receipt, FunnelReceiptIds},
          {k_mb_oneapi_receipt, ONEAPIReceiptIds},
          {k_mb_incoming_sms, IncomingSmsIds}]}.

-spec get_item(ItemType::atom(), ItemID::binary()) -> Item::tuple().
get_item(k_mb_oneapi_receipt, ID) ->
    {ok, [{_, Doc}]} = mongodb_storage:find(mailbox_storage, oneapi_receipts, {'_id' , ID}),
    {ok, #k_mb_oneapi_receipt{
        id = ID,
        customer_id = bsondoc:at(customer_id, Doc),
        user_id = bsondoc:at(user_id, Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
        req_id = bsondoc:at(req_id, Doc),
        in_msg_id = bsondoc:at(in_msg_id, Doc),
        submit_date = bsondoc:at(submit_date, Doc),
        done_date = bsondoc:at(done_date, Doc),
        status = bsondoc:binary_to_atom(bsondoc:at(status, Doc)),
        delivery_attempt = bsondoc:at(delivery_attempt, Doc),
        created_at = bsondoc:at(created_at, Doc)
    }};
get_item(k_mb_funnel_receipt, ID) ->
    case mongodb_storage:find(mailbox_storage, funnel_receipts, {'_id' , ID}) of
        {ok, [{_, Doc}]} ->
            {ok, #k_mb_funnel_receipt{
                id = ID,
                customer_id = bsondoc:at(customer_id, Doc),
                user_id = bsondoc:at(user_id, Doc),
                src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
                dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
                req_id = bsondoc:at(req_id, Doc),
                in_msg_id = bsondoc:at(in_msg_id, Doc),
                submit_date = bsondoc:at(submit_date, Doc),
                done_date = bsondoc:at(done_date, Doc),
                status = bsondoc:binary_to_atom(bsondoc:at(status, Doc)),
                delivery_attempt = bsondoc:at(delivery_attempt, Doc),
                created_at = bsondoc:at(created_at, Doc)
            }};
        _ -> no_record
    end;
get_item(k_mb_incoming_sms, ID) ->
    {ok, [{_, Doc}]} = mongodb_storage:find(mailbox_storage, incoming_sms, {'_id' , ID}),
    {ok, #k_mb_incoming_sms{
        id = ID,
        customer_id = bsondoc:at(customer_id, Doc),
        user_id = bsondoc:at(user_id, Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
        received = bsondoc:at(received, Doc),
        body = bsondoc:at(body, Doc),
        encoding = bsondoc:binary_to_atom(bsondoc:at(encoding, Doc)),
        delivery_attempt = bsondoc:at(delivery_attempt, Doc),
        created_at = bsondoc:at(created_at, Doc)
    }}.

-spec get_subscription_for_oneapi_receipt(Receipt::#k_mb_oneapi_receipt{}) ->
    undefined |
    {ok, k_mb_subscription()}.
get_subscription_for_oneapi_receipt(R = #k_mb_oneapi_receipt{}) ->
    ReqId = R#k_mb_oneapi_receipt.req_id,
    InMsgId = R#k_mb_oneapi_receipt.in_msg_id,
    Selector = {
        'req_id'    , ReqId,
        'in_msg_ids', InMsgId
    },
    case mongodb_storage:find(mailbox_storage, oneapi_receipt_subs, Selector) of
        {ok, []} ->
            CustomerId = R#k_mb_oneapi_receipt.customer_id,
            UserId = R#k_mb_oneapi_receipt.user_id,
            Selector2 = {
                'customer_id', CustomerId,
                'user_id'    , UserId,
                'type'       , bsondoc:atom_to_binary(k_mb_oneapi_receipt_sub)
            },
            case mongodb_storage:find(mailbox_storage, subscriptions, Selector2) of
                {ok, []} ->
                    undefined;
                {ok, [{_, SubDoc} | _]} ->
                    Sub = #k_mb_oneapi_receipt_sub{
                        id = bsondoc:at('_id', SubDoc),
                        customer_id = bsondoc:at(customer_id, SubDoc),
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
                customer_id = bsondoc:at(customer_id, SubDoc),
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

-spec get_subscription(SubscriptionID::binary()) ->
    {ok, k_mb_subscription()}.
get_subscription(SubscriptionID) ->
    {ok, [{_, Doc}]} = mongodb_storage:find(mailbox_storage, subscriptions, {'_id' , SubscriptionID}),
    {ok, get_subscription(bsondoc:binary_to_atom(bsondoc:at(type, Doc)), SubscriptionID, Doc)}.

get_subscription(k_mb_oneapi_receipt_sub, ID, Doc) ->
    #k_mb_oneapi_receipt_sub{
        id = ID,
        customer_id = bsondoc:at(customer_id, Doc),
        user_id = bsondoc:at(user_id, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
        notify_url = bsondoc:at(notify_url, Doc),
        callback_data = bsondoc:at(callback_data, Doc),
        created_at = bsondoc:at(created_at, Doc)
    };
get_subscription(k_mb_oneapi_incoming_sms_sub, ID, Doc) ->
    #k_mb_oneapi_incoming_sms_sub{
        id = ID,
        customer_id = bsondoc:at(customer_id, Doc),
        user_id = bsondoc:at(user_id, Doc),
        priority = bsondoc:at(priority, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
        notify_url = bsondoc:at(notify_url, Doc),
        criteria = bsondoc:at(criteria, Doc),
        callback_data = bsondoc:at(callback_data, Doc),
        created_at = bsondoc:at(created_at, Doc)
    };
get_subscription(k_mb_funnel_sub, ID, Doc) ->
    #k_mb_funnel_sub{
        id = ID,
        customer_id = bsondoc:at(customer_id, Doc),
        user_id = bsondoc:at(user_id, Doc),
        priority = bsondoc:at(priority, Doc),
        queue_name = bsondoc:at(queue_name, Doc),
        created_at = bsondoc:at(created_at, Doc)
    }.

-spec get_funnel_subscriptions() -> {ok, [#k_mb_funnel_sub{}]}.
get_funnel_subscriptions() ->
    Selector = {'type' , bsondoc:atom_to_binary(k_mb_funnel_sub)},
    {ok, Docs} =
        mongodb_storage:find(mailbox_storage, subscriptions, Selector),
    Subs = [get_subscription(k_mb_funnel_sub, ID, Doc) || {ID, Doc} <- Docs],
    {ok, Subs}.

-spec get_subscription_ids() -> {ok, [binary()]}.
get_subscription_ids() ->
    {ok, Docs} = mongodb_storage:find(mailbox_storage, subscriptions, {}, {'_id', 1}),
    IDs = [ID || {ID, _} <- Docs],
    {ok, IDs}.

-spec set_pending(atom(), binary(), binary(), binary()) -> ok.
set_pending(ItemType, ItemID, CustomerID, UserID) ->
    Selector = {'_id' , ItemID},
    Modifier = {
        '$set', {
            'type'       , bsondoc:atom_to_binary(ItemType),
            'customer_id', CustomerID,
            'user_id'    , UserID
        }
    },
    ok = mongodb_storage:upsert(mailbox_storage, pending_items, Selector, Modifier).

-spec get_pending(CustomerID::binary(), UserID::binary()) ->
    {ok, []} | {ok, [{ItemType::atom(), ItemID::binary()}]}.
get_pending(CustomerID, UserID) ->
    Selector = {
        'customer_id', CustomerID,
        'user_id'    , UserID
    },
    {ok, Docs} = mongodb_storage:find(mailbox_storage, pending_items, Selector),
    Items = [{bsondoc:binary_to_atom(bsondoc:at(type, Doc)), ID} || {ID, Doc} <- Docs],
    {ok, Items}.

-spec get_incoming_sms(binary(), binary(), addr(), integer() | undefined) ->
    {ok, [#k_mb_incoming_sms{}], Total::integer()}.
get_incoming_sms(CustomerID, UserID, DstAddr, Limit) ->
    Selector = {
        'customer_id', CustomerID,
        'user_id'    , UserID,
        'dst_addr'   , k_storage_utils:addr_to_doc(DstAddr)
    },
    {ok, Docs} = mongodb_storage:find(mailbox_storage, incoming_sms, Selector),
    AllItems =
        [#k_mb_incoming_sms{
            id = bsondoc:at('_id', Doc),
            customer_id = bsondoc:at(customer_id, Doc),
            user_id = bsondoc:at(user_id, Doc),
            src_addr = k_storage_utils:doc_to_addr(bsondoc:at(src_addr, Doc)),
            dst_addr = k_storage_utils:doc_to_addr(bsondoc:at(dst_addr, Doc)),
            received = bsondoc:at(received, Doc),
            body = bsondoc:at(body, Doc),
            encoding = bsondoc:binary_to_atom(bsondoc:at(encoding, Doc)),
            delivery_attempt = bsondoc:at(delivery_attempt, Doc),
            created_at = bsondoc:at(created_at, Doc)
        } || {_, Doc} <- Docs],
    Items = head(Limit, AllItems),
    Pending = length(AllItems) - length(Items),
    {ok, Items, Pending}.

%% ===================================================================
%% Internal
%% ===================================================================

head(undefined, List) ->
    head(100, List);
head(N, List) ->
    lists:sublist(List, N).
