-module(k_mb_db).

-include_lib("k_common/include/logging.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("application.hrl").
-include("address.hrl").

-type input_sms_id() :: {	CustomerID :: binary(),
							ClientType :: atom(),
							MessageID :: bitstring()
						}.
%% API
-export([
	save/1,
	save_sub/1,

	get_subscription/1,
	get_funnel_subscriptions/0,
	get_subscription_for_k1api_receipt/1,
	get_subscription_ids/0,
	delete_subscription/1,

	get_items/0,
	get_item/2,
	get_funnel_receipts/2,
	delete_item/1,

	set_pending/4,
	get_pending/2,

	get_incoming_sms/4,

	link_input_id_to_sub_id/2
]).

%% ===================================================================
%% API
%% ===================================================================

-spec save(tuple()) -> ok.
save(#k_mb_k1api_receipt_sub{} = Sub) ->
	Selector = {
		'_id' , Sub#k_mb_k1api_receipt_sub.id
	},
	Modifier = {
		'$set' , {
			'customer_id'   , Sub#k_mb_k1api_receipt_sub.customer_id,
			'user_id'       , Sub#k_mb_k1api_receipt_sub.user_id,
			'queue_name'    , Sub#k_mb_k1api_receipt_sub.queue_name,
			'dest_addr'     , k_storage_utils:addr_to_doc(Sub#k_mb_k1api_receipt_sub.dest_addr),
			'notify_url'    , Sub#k_mb_k1api_receipt_sub.notify_url,
			'callback_data' , Sub#k_mb_k1api_receipt_sub.callback_data,
			'created_at'    , Sub#k_mb_k1api_receipt_sub.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?k1apiReceiptSubColl, Selector, Modifier);
save(#k_mb_incoming_sms{} = Sms) ->
	Selector = {
		'_id' , Sms#k_mb_incoming_sms.id
	},
	Modifier = {
		'$set' , {
			'customer_id' , Sms#k_mb_incoming_sms.customer_id,
			'user_id' , Sms#k_mb_incoming_sms.user_id,
			'source_addr' , k_storage_utils:addr_to_doc(Sms#k_mb_incoming_sms.source_addr),
			'dest_addr' , k_storage_utils:addr_to_doc(Sms#k_mb_incoming_sms.dest_addr),
			'received' , Sms#k_mb_incoming_sms.received,
			'message_body' , Sms#k_mb_incoming_sms.message_body,
			'encoding' , bsondoc:atom_to_binary(Sms#k_mb_incoming_sms.encoding),
			'delivery_attempt' , Sms#k_mb_incoming_sms.delivery_attempt,
			'created_at' , Sms#k_mb_incoming_sms.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?incomingSmsColl, Selector, Modifier);
save(#k_mb_k1api_receipt{} = R) ->
	Selector = {
		'_id' , R#k_mb_k1api_receipt.id
	},
	Modifier = {
		'$set' , {
			'customer_id'      , R#k_mb_k1api_receipt.customer_id,
			'user_id'          , R#k_mb_k1api_receipt.user_id,
			'source_addr'      , k_storage_utils:addr_to_doc(R#k_mb_k1api_receipt.source_addr),
			'dest_addr'        , k_storage_utils:addr_to_doc(R#k_mb_k1api_receipt.dest_addr),
			'input_message_id' , R#k_mb_k1api_receipt.input_message_id,
			'message_state'    , R#k_mb_k1api_receipt.message_state,
			'delivery_attempt' , R#k_mb_k1api_receipt.delivery_attempt,
			'created_at'       , R#k_mb_k1api_receipt.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?k1apiReceiptsColl, Selector, Modifier);
save(#k_mb_funnel_receipt{} = R) ->
	Selector = {
		'_id' , R#k_mb_funnel_receipt.id
	},
	Modifier = {
		'$set' , {
			'customer_id'      , R#k_mb_funnel_receipt.customer_id,
			'user_id'          , R#k_mb_funnel_receipt.user_id,
			'source_addr'      , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.source_addr),
			'dest_addr'        , k_storage_utils:addr_to_doc(R#k_mb_funnel_receipt.dest_addr),
			'input_message_id' , R#k_mb_funnel_receipt.input_message_id,
			'submit_date'      , R#k_mb_funnel_receipt.submit_date,
			'done_date'        , R#k_mb_funnel_receipt.done_date,
			'message_state'    , bsondoc:atom_to_binary(R#k_mb_funnel_receipt.message_state),
			'delivery_attempt' , R#k_mb_funnel_receipt.delivery_attempt,
			'created_at'       , R#k_mb_funnel_receipt.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?funnelReceiptsColl, Selector, Modifier).

save_sub(#k_mb_k1api_receipt_sub{} = Sub) ->
	Selector = {
		'_id' , Sub#k_mb_k1api_receipt_sub.id
	},
	Modifier = {
		'$set' , {
			'type'          , bsondoc:atom_to_binary(k_mb_k1api_receipt_sub),
			'customer_id'   , Sub#k_mb_k1api_receipt_sub.customer_id,
			'user_id'       , Sub#k_mb_k1api_receipt_sub.user_id,
			'queue_name'    , Sub#k_mb_k1api_receipt_sub.queue_name,
			'dest_addr'     , k_storage_utils:addr_to_doc(Sub#k_mb_k1api_receipt_sub.dest_addr),
			'notify_url'    , Sub#k_mb_k1api_receipt_sub.notify_url,
			'callback_data' , Sub#k_mb_k1api_receipt_sub.callback_data,
			'created_at'    , Sub#k_mb_k1api_receipt_sub.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?subscriptionsColl, Selector, Modifier);
save_sub(#k_mb_k1api_incoming_sms_sub{} = Sub) ->
	Selector = {
		'_id' , Sub#k_mb_k1api_incoming_sms_sub.id
	},
	Modifier = {
		'$set' , {
			'type'          , bsondoc:atom_to_binary(k_mb_k1api_incoming_sms_sub),
			'customer_id'   , Sub#k_mb_k1api_incoming_sms_sub.customer_id,
			'user_id'       , Sub#k_mb_k1api_incoming_sms_sub.user_id,
			'priority'      , Sub#k_mb_k1api_incoming_sms_sub.priority,
			'queue_name'    , Sub#k_mb_k1api_incoming_sms_sub.queue_name,
			'dest_addr'     , k_storage_utils:addr_to_doc(Sub#k_mb_k1api_incoming_sms_sub.dest_addr),
			'notify_url'    , Sub#k_mb_k1api_incoming_sms_sub.notify_url,
			'criteria'      , Sub#k_mb_k1api_incoming_sms_sub.criteria,
			'callback_data' , Sub#k_mb_k1api_incoming_sms_sub.callback_data,
			'created_at'    , Sub#k_mb_k1api_incoming_sms_sub.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?subscriptionsColl, Selector, Modifier);
save_sub(#k_mb_funnel_sub{} = Sub) ->
	Selector = {
		'_id' , Sub#k_mb_funnel_sub.id
	},
	Modifier = {
		'$set' , {
			'type'        , bsondoc:atom_to_binary(k_mb_funnel_sub),
			'customer_id' , Sub#k_mb_funnel_sub.customer_id,
			'user_id'     , Sub#k_mb_funnel_sub.user_id,
			'priority'    , Sub#k_mb_funnel_sub.priority,
			'queue_name'  , Sub#k_mb_funnel_sub.queue_name,
			'created_at'  , Sub#k_mb_funnel_sub.created_at
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?subscriptionsColl, Selector, Modifier).

-spec delete_subscription(SubscriptionID :: binary()) -> ok.
delete_subscription(SubscriptionID) ->
	ok = mongodb_storage:delete(k_static_storage, ?subscriptionsColl, {'_id' , SubscriptionID}).

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item = #k_mb_funnel_receipt{}) ->
	Selector = {'_id', Item#k_mb_funnel_receipt.id},
	ok = mongodb_storage:delete(k_static_storage, ?funnelReceiptsColl, Selector);
delete_item(Item = #k_mb_k1api_receipt{}) ->
	Selector = {
		'_id'         , Item#k_mb_k1api_receipt.id,
		'customer_id' , Item#k_mb_k1api_receipt.customer_id,
		'user_id'     , Item#k_mb_k1api_receipt.user_id
	},
	ok = mongodb_storage:delete(k_static_storage, ?k1apiReceiptsColl, Selector),
	ok = mongodb_storage:delete(k_static_storage, ?pendingItemsColl, Selector);
delete_item(Item = #k_mb_incoming_sms{}) ->
	Selector = {
		'_id'         , Item#k_mb_incoming_sms.id,
		'customer_id' , Item#k_mb_incoming_sms.customer_id,
		'user_id'     , Item#k_mb_incoming_sms.user_id
	},
	ok = mongodb_storage:delete(k_static_storage, ?incomingSmsColl, Selector),
	ok = mongodb_storage:delete(k_static_storage, ?pendingItemsColl, Selector).

-spec get_funnel_receipts(binary(), binary()) -> {ok, [{k_mb_funnel_receipt, ID :: binary()}]}.
get_funnel_receipts(CustomerID, UserID) ->
	Selector = {
		customer_id, CustomerID,
		user_id, UserID
	},
	{ok, FunnelReceiptDocs} = mongodb_storage:find(k_static_storage, ?funnelReceiptsColl, Selector, {'_id' , 1}),
	FunnelReceipts = [{k_mb_funnel_receipt, RID} || {RID, _} <- FunnelReceiptDocs],
	{ok, FunnelReceipts}.

-spec get_items() -> {ok, [binary()]}.
get_items() ->
	{ok, FunnelReceiptDocs} = mongodb_storage:find(k_static_storage, ?funnelReceiptsColl, {}, {'_id' , 1}),
	FunnelReceiptIds = [RID || {RID, _} <- FunnelReceiptDocs],
	{ok, K1apiReceiptDocs} = mongodb_storage:find(k_static_storage, ?k1apiReceiptsColl, {}, {'_id' , 1}),
	K1APIReceiptIds = [RID || {RID, _} <- K1apiReceiptDocs],
	{ok, IncomingSmsDocs} = mongodb_storage:find(k_static_storage, ?incomingSmsColl, {}, {'_id' , 1}),
	IncomingSmsIds = [ISID || {ISID, _} <- IncomingSmsDocs],
	{ok, [	{k_mb_funnel_receipt, FunnelReceiptIds},
			{k_mb_k1api_receipt, K1APIReceiptIds},
			{k_mb_incoming_sms, IncomingSmsIds}	]}.

-spec get_item(ItemType :: atom(), ItemID :: binary()) -> Item :: tuple().
get_item(k_mb_k1api_receipt, ID) ->
	{ok, [{_, Doc}]} = mongodb_storage:find(k_static_storage, ?k1apiReceiptsColl, {'_id' , ID}),
	{ok, #k_mb_k1api_receipt{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		source_addr = k_storage_utils:doc_to_addr(bsondoc:at(source_addr, Doc)),
		dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
		input_message_id = bsondoc:at(input_message_id, Doc),
		message_state = bsondoc:binary_to_atom(bsondoc:at(message_state, Doc)),
		delivery_attempt = bsondoc:at(delivery_attempt, Doc),
		created_at = bsondoc:at(created_at, Doc)
	}};
get_item(k_mb_funnel_receipt, ID) ->
	case mongodb_storage:find(k_static_storage, ?funnelReceiptsColl, {'_id' , ID}) of
		{ok, [{_, Doc}]} ->
			{ok, #k_mb_funnel_receipt{
				id = ID,
				customer_id = bsondoc:at(customer_id, Doc),
				user_id = bsondoc:at(user_id, Doc),
				source_addr = k_storage_utils:doc_to_addr(bsondoc:at(source_addr, Doc)),
				dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
				input_message_id = bsondoc:at(input_message_id, Doc),
				submit_date = bsondoc:at(submit_date, Doc),
				done_date = bsondoc:at(done_date, Doc),
				message_state = bsondoc:binary_to_atom(bsondoc:at(message_state, Doc)),
				delivery_attempt = bsondoc:at(delivery_attempt, Doc),
				created_at = bsondoc:at(created_at, Doc)
			}};
		_ -> no_record
	end;
get_item(k_mb_incoming_sms, ID) ->
	{ok, [{_, Doc}]} = mongodb_storage:find(k_static_storage, ?incomingSmsColl, {'_id' , ID}),
	{ok, #k_mb_incoming_sms{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		source_addr = k_storage_utils:doc_to_addr(bsondoc:at(source_addr, Doc)),
		dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
		received = bsondoc:at(received, Doc),
		message_body = bsondoc:at(message_body, Doc),
		encoding = bsondoc:binary_to_atom(bsondoc:at(encoding, Doc)),
		delivery_attempt = bsondoc:at(delivery_attempt, Doc),
		created_at = bsondoc:at(created_at, Doc)
	}}.

-spec get_subscription_for_k1api_receipt(Receipt :: #k_mb_k1api_receipt{}) ->
	undefined |
	{ok, k_mb_subscription()}.
get_subscription_for_k1api_receipt(Receipt = #k_mb_k1api_receipt{}) ->
	MessageID = Receipt#k_mb_k1api_receipt.input_message_id,
	CustomerID = Receipt#k_mb_k1api_receipt.customer_id,
	%% ClientType = bsondoc:atom_to_binary(k1api),
	InputID = {CustomerID, <<"k1api">>, MessageID},
	?log_debug("InputID: ~p", [InputID]),
	Selector = {
		'customer_id' , CustomerID,
		'client_type' , <<"k1api">>,
		'input_id'    , MessageID
	},
	case mongodb_storage:find(k_static_storage, ?inputIdToSubIdColl, Selector) of
		{ok, []} ->
			?log_warn("k1api InputID undefined", []),
			undefined;
		{ok, [{_, Doc}]} ->
			?log_debug("Doc: ~p", [Doc]),
			SubID = bsondoc:at(subscription_id, Doc),
			?log_debug("SubID: ~p", [SubID]),
			{ok, [{_, SubDoc}]} = mongodb_storage:find(k_static_storage, ?k1apiReceiptSubColl, {'_id' , SubID}),
			Sub = #k_mb_k1api_receipt_sub{
				id = SubID,
				customer_id = bsondoc:at(customer_id, SubDoc),
				user_id = bsondoc:at(user_id, SubDoc),
				queue_name = bsondoc:at(queue_name, SubDoc),
				dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, SubDoc)),
				notify_url = bsondoc:at(notify_url, SubDoc),
				callback_data = bsondoc:at(callback_data, SubDoc),
				created_at = bsondoc:at(created_at, SubDoc)
			},
			?log_debug("FOUND suitable subscription: ~p", [Sub]),
			{ok, Sub}
	end.

-spec get_subscription(SubscriptionID :: binary()) ->
	{ok, k_mb_subscription()}.
get_subscription(SubscriptionID) ->
	{ok, [{_, Doc}]} = mongodb_storage:find(k_static_storage, ?subscriptionsColl, {'_id' , SubscriptionID}),
	get_subscription(bsondoc:binary_to_atom(bsondoc:at(type, Doc)), SubscriptionID, Doc).

get_subscription(k_mb_k1api_receipt_sub, ID, Doc) ->
	{ok, #k_mb_k1api_receipt_sub{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		queue_name = bsondoc:at(queue_name, Doc),
		dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
		notify_url = bsondoc:at(notify_url, Doc),
		callback_data = bsondoc:at(callback_data, Doc),
		created_at = bsondoc:at(created_at, Doc)
	}};
get_subscription(k_mb_k1api_incoming_sms_sub, ID, Doc) ->
	{ok, #k_mb_k1api_incoming_sms_sub{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		priority = bsondoc:at(priority, Doc),
		queue_name = bsondoc:at(queue_name, Doc),
		dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
		notify_url = bsondoc:at(notify_url, Doc),
		criteria = bsondoc:at(criteria, Doc),
		callback_data = bsondoc:at(callback_data, Doc),
		created_at = bsondoc:at(created_at, Doc)
	}};
get_subscription(k_mb_funnel_sub, ID, Doc) ->
	{ok, #k_mb_funnel_sub{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		priority = bsondoc:at(priority, Doc),
		queue_name = bsondoc:at(queue_name, Doc),
		created_at = bsondoc:at(created_at, Doc)
	}}.

-spec get_funnel_subscriptions() -> {ok, [#k_mb_funnel_sub{}]}.
get_funnel_subscriptions() ->
	{ok, Docs} =
		mongodb_storage:find(k_static_storage, ?subscriptionsColl, {'type' , <<"k_mb_funnel_sub">>}),
	Subs = [get_subscription(k_mb_funnel_sub, ID, Doc) || {ID, Doc} <- Docs],
	{ok, Subs}.

-spec get_subscription_ids() -> {ok, [binary()]}.
get_subscription_ids() ->
	{ok, Docs} = mongodb_storage:find(k_static_storage, ?subscriptionsColl, {}, {'_id', 1}),
	IDs = [ID || {ID, _} <- Docs],
	{ok, IDs}.

-spec set_pending(atom(), binary(), binary(), bitstring()) -> ok.
set_pending(ItemType, ItemID, CustomerID, UserID) ->
	Selector = {'_id' , ItemID},
	Modifier = {
		'$set' , {
			'type'        , bsondoc:atom_to_binary(ItemType),
			'customer_id' , CustomerID,
			'user_id'     , UserID
		}
	},
	ok = mongodb_storage:upsert(k_static_storage, ?pendingItemsColl, Selector, Modifier).

-spec get_pending(CustomerID :: binary(), UserID :: bitstring()) ->
	{ok, []} | {ok, [{ItemType :: atom(), ItemID :: binary()}]}.
get_pending(CustomerID, UserID) ->
	Selector = {
		'customer_id' , CustomerID,
		'user_id'     , UserID
	},
	{ok, Docs} = mongodb_storage:find(k_static_storage, ?pendingItemsColl, Selector),
	Items = [{bsondoc:binary_to_atom(bsondoc:at(type, Doc)), ID} || {ID, Doc} <- Docs],
	{ok, Items}.

-spec get_incoming_sms(binary(), bitstring(), addr(), integer() | undefined) ->
	{ok, [#k_mb_incoming_sms{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestinationAddr, Limit) ->
	Selector = {
		'customer_id' , CustomerID,
		'user_id'     , UserID,
		'dest_addr'   , k_storage_utils:addr_to_doc(DestinationAddr)
	},
	{ok, ISDocs} = mongodb_storage:find(k_static_storage, ?incomingSmsColl, Selector),
	AllItems =
	[#k_mb_incoming_sms{
		id = ID,
		customer_id = bsondoc:at(customer_id, Doc),
		user_id = bsondoc:at(user_id, Doc),
		source_addr = k_storage_utils:doc_to_addr(bsondoc:at(source_addr, Doc)),
		dest_addr = k_storage_utils:doc_to_addr(bsondoc:at(dest_addr, Doc)),
		received = bsondoc:at(received, Doc),
		message_body = bsondoc:at(message_body, Doc),
		encoding = bsondoc:binary_to_atom(bsondoc:at(encoding, Doc)),
		delivery_attempt = bsondoc:at(delivery_attempt, Doc),
		created_at = bsondoc:at(created_at, Doc)
	} || {ID, Doc} <- ISDocs],
	Total = length(AllItems),
	Items = first(AllItems, Limit),
	{ok, Items, Total}.

-spec link_input_id_to_sub_id(InputID :: input_sms_id(), SubscriptionID :: binary()) -> ok.
link_input_id_to_sub_id({CID, k1api, ID} = _InputID, SubscriptionID) -> %% <- add user_name field
	Modifier = {
		'customer_id'     , CID,
		'client_type'     , bsondoc:atom_to_binary(k1api),
		'input_id'        , ID,
		'subscription_id' , SubscriptionID
	},
	{ok, _ID} = mongodb_storage:insert(k_static_storage, ?inputIdToSubIdColl, Modifier),
	ok.

%% ===================================================================
%% Internal
%% ===================================================================

first(Limit, undefined) ->
	first(Limit, 100);
first(List, Limit) ->
	first(List, Limit, []).
first([], _Counter, Acc) ->
	lists:reverse(Acc);
first(_List, 0, Acc) ->
	lists:reverse(Acc);
first([Elem | Rest], Counter, Acc) ->
	first(Rest, Counter - 1, [Elem | Acc]).
