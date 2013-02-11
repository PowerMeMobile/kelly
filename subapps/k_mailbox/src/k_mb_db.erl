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
	get_subscription_for_k1api_receipt/1,
	get_subscription_ids/0,
	delete_subscription/1,

	get_items/0,
	get_item/2,
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
	Selector = [{'_id', Sub#k_mb_k1api_receipt_sub.id}],
	Plist = [
		{customer_id, Sub#k_mb_k1api_receipt_sub.customer_id},
		{user_id, Sub#k_mb_k1api_receipt_sub.user_id},
		{queue_name, Sub#k_mb_k1api_receipt_sub.queue_name},
		{dest_addr, k_storage:addr_to_doc(Sub#k_mb_k1api_receipt_sub.dest_addr)},
		{notify_url, Sub#k_mb_k1api_receipt_sub.notify_url},
		{callback_data, Sub#k_mb_k1api_receipt_sub.callback_data},
		{created_at, Sub#k_mb_k1api_receipt_sub.created_at}
	],
	ok = mongodb_storage:upsert(?k1apiReceiptSubColl, Selector, Plist);
save(#k_mb_incoming_sms{} = Sms) ->
	Selector = [{'_id', Sms#k_mb_incoming_sms.id}],
	Plist = [
		{customer_id, Sms#k_mb_incoming_sms.customer_id},
		{user_id, Sms#k_mb_incoming_sms.user_id},
		{source_addr, k_storage:addr_to_doc(Sms#k_mb_incoming_sms.source_addr)},
		{dest_addr, k_storage:addr_to_doc(Sms#k_mb_incoming_sms.dest_addr)},
		{received, Sms#k_mb_incoming_sms.received},
		{message_body, Sms#k_mb_incoming_sms.message_body},
		{encoding, Sms#k_mb_incoming_sms.encoding},
		{delivery_attempt, Sms#k_mb_incoming_sms.delivery_attempt},
		{created_at, Sms#k_mb_incoming_sms.created_at}
	],
	ok = mongodb_storage:upsert(?incomingSmsColl, Selector, Plist);
save(#k_mb_k1api_receipt{} = R) ->
	Selector = [{'_id', R#k_mb_k1api_receipt.id}],
	Plist = [
		{customer_id, R#k_mb_k1api_receipt.customer_id},
		{user_id, R#k_mb_k1api_receipt.user_id},
		{source_addr, k_storage:addr_to_doc(R#k_mb_k1api_receipt.source_addr)},
		{dest_addr, k_storage:addr_to_doc(R#k_mb_k1api_receipt.dest_addr)},
		{input_message_id, R#k_mb_k1api_receipt.input_message_id},
		{message_state, R#k_mb_k1api_receipt.message_state},
		{delivery_attempt, R#k_mb_k1api_receipt.delivery_attempt},
		{created_at, R#k_mb_k1api_receipt.created_at}
	],
	ok = mongodb_storage:upsert(?k1apiReceiptsColl, Selector, Plist);
save(#k_mb_funnel_receipt{} = R) ->
	Selector = [{'_id', R#k_mb_funnel_receipt.id}],
	Plist = [
		{customer_id, R#k_mb_funnel_receipt.customer_id},
		{user_id, R#k_mb_funnel_receipt.user_id},
		{source_addr, k_storage:addr_to_doc(R#k_mb_funnel_receipt.source_addr)},
		{dest_addr, k_storage:addr_to_doc(R#k_mb_funnel_receipt.dest_addr)},
		{input_message_id, R#k_mb_funnel_receipt.input_message_id},
		{submit_date, R#k_mb_funnel_receipt.submit_date},
		{done_date, R#k_mb_funnel_receipt.done_date},
		{message_state, R#k_mb_funnel_receipt.message_state},
		{delivery_attempt, R#k_mb_funnel_receipt.delivery_attempt},
		{created_at, R#k_mb_funnel_receipt.created_at}
	],
	ok = mongodb_storage:upsert(?funnelReceiptsColl, Selector, Plist);
save(Record) ->
	ok = mnesia:dirty_write(Record).

save_sub(#k_mb_k1api_receipt_sub{} = Sub) ->
	Selector = [{'_id', Sub#k_mb_k1api_receipt_sub.id}],
	Plist = [
		{type, k_mb_k1api_receipt_sub},
		{customer_id, Sub#k_mb_k1api_receipt_sub.customer_id},
		{user_id, Sub#k_mb_k1api_receipt_sub.user_id},
		{queue_name, Sub#k_mb_k1api_receipt_sub.queue_name},
		{dest_addr, k_storage:addr_to_doc(Sub#k_mb_k1api_receipt_sub.dest_addr)},
		{notify_url, Sub#k_mb_k1api_receipt_sub.notify_url},
		{callback_data, Sub#k_mb_k1api_receipt_sub.callback_data},
		{created_at, Sub#k_mb_k1api_receipt_sub.created_at}
	],
	ok = mongodb_storage:upsert(?subscriptionsColl, Selector, Plist);
save_sub(#k_mb_k1api_incoming_sms_sub{} = Sub) ->
	Selector = [{'_id', Sub#k_mb_k1api_incoming_sms_sub.id}],
	Plist = [
		{type, k_mb_k1api_incoming_sms_sub},
		{customer_id, Sub#k_mb_k1api_incoming_sms_sub.customer_id},
		{user_id, Sub#k_mb_k1api_incoming_sms_sub.user_id},
		{priority, Sub#k_mb_k1api_incoming_sms_sub.priority},
		{queue_name, Sub#k_mb_k1api_incoming_sms_sub.queue_name},
		{dest_addr, k_storage:addr_to_doc(Sub#k_mb_k1api_incoming_sms_sub.dest_addr)},
		{notify_url, Sub#k_mb_k1api_incoming_sms_sub.notify_url},
		{criteria, Sub#k_mb_k1api_incoming_sms_sub.criteria},
		{callback_data, Sub#k_mb_k1api_incoming_sms_sub.callback_data},
		{created_at, Sub#k_mb_k1api_incoming_sms_sub.created_at}
	],
	ok = mongodb_storage:upsert(?subscriptionsColl, Selector, Plist);
save_sub(#k_mb_funnel_sub{} = Sub) ->
	Selector = [{'_id', Sub#k_mb_funnel_sub.id}],
	Plist = [
		{type, k_mb_funnel_sub},
		{customer_id, Sub#k_mb_funnel_sub.customer_id},
		{user_id, Sub#k_mb_funnel_sub.user_id},
		{priority, Sub#k_mb_funnel_sub.priority},
		{queue_name, Sub#k_mb_funnel_sub.queue_name},
		{created_at, Sub#k_mb_funnel_sub.created_at}
	],
	ok = mongodb_storage:upsert(?subscriptionsColl, Selector, Plist).

-spec delete_subscription(SubscriptionID :: binary()) -> ok.
delete_subscription(SubscriptionID) ->
	ok = mongodb_storage:delete(?subscriptionsColl, [{'_id', SubscriptionID}]).

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item = #k_mb_funnel_receipt{}) ->
	Selector = [
		{'_id', Item#k_mb_funnel_receipt.id},
		{customer_id, Item#k_mb_funnel_receipt.customer_id},
		{user_id, Item#k_mb_funnel_receipt.user_id}
	],
	ok = mongodb_storage:delete(?funnelReceiptsColl, Selector),
	ok = mongodb_storage:delete(?pendingItemsColl, Selector);
delete_item(Item = #k_mb_k1api_receipt{}) ->
	Selector = [
		{'_id', Item#k_mb_k1api_receipt.id},
		{customer_id, Item#k_mb_k1api_receipt.customer_id},
		{user_id, Item#k_mb_k1api_receipt.user_id}
	],
	ok = mongodb_storage:delete(?k1apiReceiptsColl, Selector),
	ok = mongodb_storage:delete(?pendingItemsColl, Selector);
delete_item(Item = #k_mb_incoming_sms{}) ->
	Selector = [
		{'_id', Item#k_mb_incoming_sms.id},
		{customer_id, Item#k_mb_incoming_sms.customer_id},
		{user_id, Item#k_mb_incoming_sms.user_id}
	],
	ok = mongodb_storage:delete(?incomingSmsColl, Selector),
	ok = mongodb_storage:delete(?pendingItemsColl, Selector).

-spec get_items() -> {ok, [binary()]}.
get_items() ->
	{ok, FunnelReceiptDocs} = mongodb_storage:find(?funnelReceiptsColl, [], [{'_id', 1}]),
	FunnelReceiptIds = [RID || {RID, _} <- FunnelReceiptDocs],
	{ok, K1apiReceiptDocs} = mongodb_storage:find(?k1apiReceiptsColl, [], [{'_id', 1}]),
	K1APIReceiptIds = [RID || {RID, _} <- K1apiReceiptDocs],
	{ok, IncomingSmsDocs} = mongodb_storage:find(?incomingSmsColl, [], [{'_id', 1}]),
	IncomingSmsIds = [ISID || {ISID, _} <- IncomingSmsDocs],
	{ok, [	{k_mb_funnel_receipt, FunnelReceiptIds},
			{k_mb_k1api_receipt, K1APIReceiptIds},
			{k_mb_incoming_sms, IncomingSmsIds}	]}.

-spec get_item(ItemType :: atom(), ItemID :: binary()) -> Item :: tuple().
get_item(k_mb_k1api_receipt, ID) ->
	{ok, [{_, Plist}]} = mongodb_storage:find(?k1apiReceiptsColl, [{'_id', ID}]),
	{ok, #k_mb_k1api_receipt{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		source_addr = k_storage:doc_to_addr(proplists:get_value(source_addr, Plist)),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		input_message_id = proplists:get_value(input_message_id, Plist),
		message_state = proplists:get_value(message_state, Plist),
		delivery_attempt = proplists:get_value(delivery_attempt, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}};
get_item(k_mb_funnel_receipt, ID) ->
	{ok, [{_, Plist}]} = mongodb_storage:find(?funnelReceiptsColl, [{'_id', ID}]),
	{ok, #k_mb_funnel_receipt{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		source_addr = k_storage:doc_to_addr(proplists:get_value(source_addr, Plist)),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		input_message_id = proplists:get_value(input_message_id, Plist),
		submit_date = proplists:get_value(submit_date, Plist),
		done_date = proplists:get_value(done_date, Plist),
		message_state = proplists:get_value(message_state, Plist),
		delivery_attempt = proplists:get_value(delivery_attempt, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}};
get_item(k_mb_incoming_sms, ID) ->
	Selector = [{'_id', ID}],
	{ok, [{_, Plist}]} = mongodb_storage:find(?incomingSmsColl, Selector),
	{ok, #k_mb_incoming_sms{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		source_addr = k_storage:doc_to_addr(proplists:get_value(source_addr, Plist)),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		received = proplists:get_value(received, Plist),
		message_body = proplists:get_value(message_body, Plist),
		encoding = proplists:get_value(encoding, Plist),
		delivery_attempt = proplists:get_value(delivery_attempt, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}};
get_item(ItemType, ItemID) ->
	[Item] = mnesia:dirty_read(ItemType, ItemID),
	{ok, Item}.


-spec get_subscription_for_k1api_receipt(Receipt :: #k_mb_k1api_receipt{}) ->
	undefined |
	{ok, k_mb_subscription()}.
get_subscription_for_k1api_receipt(Receipt = #k_mb_k1api_receipt{}) ->
	MessageID = Receipt#k_mb_k1api_receipt.input_message_id,
	CustomerID = Receipt#k_mb_k1api_receipt.customer_id,
	ClientType = k1api,
	InputID = {CustomerID, ClientType, MessageID},
	?log_debug("InputID: ~p", [InputID]),
	Selector = [
		{customer_id, CustomerID},
		{client_type, ClientType},
		{input_id, MessageID}
	],
	case mongodb_storage:find(?inputIdToSubIdColl, Selector) of
		{ok, []} ->
			?log_warn("k1api InputID undefined", []),
			undefined;
		{ok, [{_, Plist}]} ->
			?log_debug("Plist: ~p", [Plist]),
			SubID = proplists:get_value(subscription_id, Plist),
			?log_debug("SubID: ~p", [SubID]),
			{ok, [{_, SubPlist}]} = mongodb_storage:find(?k1apiReceiptSubColl, [{'_id', SubID}]),
			Sub = #k_mb_k1api_receipt_sub{
				id = SubID,
				customer_id = proplists:get_value(customer_id, SubPlist),
				user_id = proplists:get_value(user_id, SubPlist),
				queue_name = proplists:get_value(queue_name, SubPlist),
				dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, SubPlist)),
				notify_url = proplists:get_value(notify_url, SubPlist),
				callback_data = proplists:get_value(callback_data, SubPlist),
				created_at = proplists:get_value(created_at, SubPlist)
			},
			?log_debug("FOUND suitable subscription: ~p", [Sub]),
			{ok, Sub}
	end.

-spec get_subscription(SubscriptionID :: binary()) ->
	{ok, k_mb_subscription()}.
get_subscription(SubscriptionID) ->
	{ok, [{_,Plist}]} = mongodb_storage:find(?subscriptionsColl, [{'_id', SubscriptionID}]),
	get_subscription(proplists:get_value(type, Plist), SubscriptionID, Plist).

get_subscription(k_mb_k1api_receipt_sub, ID, Plist) ->
	{ok, #k_mb_k1api_receipt_sub{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		queue_name = proplists:get_value(queue_name, Plist),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		notify_url = proplists:get_value(notify_url, Plist),
		callback_data = proplists:get_value(callback_data, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}};
get_subscription(k_mb_k1api_incoming_sms_sub, ID, Plist) ->
	{ok, #k_mb_k1api_incoming_sms_sub{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		priority = proplists:get_value(priority, Plist),
		queue_name = proplists:get_value(queue_name, Plist),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		notify_url = proplists:get_value(notify_url, Plist),
		criteria = proplists:get_value(criteria, Plist),
		callback_data = proplists:get_value(callback_data, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}};
get_subscription(k_mb_funnel_sub, ID, Plist) ->
	{ok, #k_mb_funnel_sub{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		priority = proplists:get_value(priority, Plist),
		queue_name = proplists:get_value(queue_name, Plist),
		created_at = proplists:get_value(created_at, Plist)
	}}.

-spec get_subscription_ids() -> {ok, [binary()]}.
get_subscription_ids() ->
	{ok, Docs} = mongodb_storage:find(?subscriptionsColl, [], [{'_id', 1}]),
	IDs = [ID || {ID, _} <- Docs],
	{ok, IDs}.

-spec set_pending(atom(), binary(), binary(), bitstring()) -> ok.
set_pending(ItemType, ItemID, CustomerID, UserID) ->
	Selector = [{'_id', ItemID}],
	Plist = [
		{type, ItemType},
		{customer_id, CustomerID},
		{user_id, UserID}
	],
	ok = mongodb_storage:upsert(?pendingItemsColl, Selector, Plist).

-spec get_pending(CustomerID :: binary(), UserID :: bitstring()) ->
	{ok, []} | {ok, [{ItemType :: atom(), ItemID :: binary()}]}.
get_pending(CustomerID, UserID) ->
	Selector = [
		{customer_id, CustomerID},
		{user_id, UserID}
	],
	{ok, Docs} = mongodb_storage:find(?pendingItemsColl, Selector),
	Items = [{proplists:get_value(type, Plist), ID} || {ID, Plist} <- Docs],
	{ok, Items}.


-spec get_incoming_sms(binary(), bitstring(), addr(), integer() | undefined) ->
	{ok, [#k_mb_incoming_sms{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestinationAddr, Limit) ->
	Selector = [
		{customer_id, CustomerID},
		{user_id, UserID},
		{dest_addr, k_storage:addr_to_doc(DestinationAddr)}
	],
	{ok, ISDocs} = mongodb_storage:find(?incomingSmsColl, Selector),
	AllItems =
	[#k_mb_incoming_sms{
		id = ID,
		customer_id = proplists:get_value(customer_id, Plist),
		user_id = proplists:get_value(user_id, Plist),
		source_addr = k_storage:doc_to_addr(proplists:get_value(source_addr, Plist)),
		dest_addr = k_storage:doc_to_addr(proplists:get_value(dest_addr, Plist)),
		received = proplists:get_value(received, Plist),
		message_body = proplists:get_value(message_body, Plist),
		encoding = proplists:get_value(encoding, Plist),
		delivery_attempt = proplists:get_value(delivery_attempt, Plist),
		created_at = proplists:get_value(created_at, Plist)
	} || {ID, Plist} <- ISDocs],
	Total = length(AllItems),
	Items = first(AllItems, Limit),
	{ok, Items, Total}.


-spec link_input_id_to_sub_id(	InputID :: input_sms_id(),
								SubscriptionID :: binary()) -> ok.
link_input_id_to_sub_id({CID, k1api, ID} = _InputID, SubscriptionID) -> %% <- add user_name field
	Plist = [
		{customer_id, CID},
		{client_type, k1api},
		{input_id, ID},
		{subscription_id, SubscriptionID}
	],
	{ok, _} = mongodb_storage:insert(?inputIdToSubIdColl, Plist),
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
