%% @private
%% @doc This module is rersponsible for all database operations.

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
	init/0,
	save/1,

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

%% @doc Sure in table existance & creates it if neccessary.
%% Used only at application initialization step.
-spec init() -> ok.
init() ->
	%% all permanent subscriptions as key - value
	ok = k_mnesia_schema:ensure_table(
		k_mb_subscription,
		record_info(fields, k_mb_subscription)),

	%% only k1api receipts for specific sms request
	ok = k_mnesia_schema:ensure_table(
		k_mb_k1api_receipt_sub,
		record_info(fields, k_mb_k1api_receipt_sub)),

	%% map k1api input sms id to specific subscription
	ok = k_mnesia_schema:ensure_table(
		k_mb_k1api_input_id_to_sub_id,
		record_info(fields, k_mb_k1api_input_id_to_sub_id)),

	ok = k_mnesia_schema:ensure_table(
		k_mb_address,
		record_info(fields, k_mb_address)),

	ok = k_mnesia_schema:ensure_table(
	    ?PENDING_ITEM,
		record_info(fields, ?PENDING_ITEM)),
	ok = k_mnesia_schema:ensure_table(
		?INCOMING_SMS,
		record_info(fields, ?INCOMING_SMS)),
	ok = k_mnesia_schema:ensure_table(
		?K1API_RECEIPT,
		record_info(fields, ?K1API_RECEIPT)),
	ok = k_mnesia_schema:ensure_table(
		?FUNNEL_RECEIPT,
		record_info(fields, ?FUNNEL_RECEIPT)).

%% @doc Save known entries in database.
-spec save(record()) -> ok.
save(Record) ->
	ok = mnesia:dirty_write(Record).

%% @doc Delete subscription from DB.
-spec delete_subscription(SubscriptionID :: binary()) -> ok.
delete_subscription(SubscriptionID) ->
	ok = mnesia:dirty_delete(k_mb_subscription, SubscriptionID).

%% @doc Delete item from db
-spec delete_item(k_mb_item()) -> ok.
delete_item(Item = #k_mb_funnel_receipt{}) ->
	delete_item(?FUNNEL_RECEIPT, Item#k_mb_funnel_receipt.id);
delete_item(Item = #k_mb_k1api_receipt{}) ->
	delete_item(?K1API_RECEIPT, Item#k_mb_k1api_receipt.id);
delete_item(Item = #k_mb_incoming_sms{}) ->
	delete_item(?INCOMING_SMS, Item#k_mb_incoming_sms.id).

delete_item(ItemType, ItemID) ->
	ok = mnesia:dirty_delete(ItemType, ItemID),
	ok = mnesia:dirty_delete(?PENDING_ITEM, {ItemType, ItemID}).

%% @doc Return all pending items from db
-spec get_items() -> {ok, [binary()]}.
get_items() ->
	FunnelReceiptIds = mnesia:dirty_all_keys(k_mb_funnel_receipt),
	K1APIReceiptIds = mnesia:dirty_all_keys(k_mb_k1api_receipt),
	IncomingSmsIds = mnesia:dirty_all_keys(k_mb_incoming_sms),
	{ok, [	{?FUNNEL_RECEIPT, FunnelReceiptIds},
			{?K1API_RECEIPT, K1APIReceiptIds},
			{?INCOMING_SMS, IncomingSmsIds}	]}.

%% @doc Return specified item from db
-spec get_item(ItemType :: atom(), ItemID :: binary()) -> Item :: record().
get_item(ItemType, ItemID) ->
	[Item] = mnesia:dirty_read(ItemType, ItemID),
	{ok, Item}.

%% @doc Return subscription by k1api receipt
-spec get_subscription_for_k1api_receipt(Receipt :: #k_mb_k1api_receipt{}) ->
	undefined |
	{ok, k_mb_subscription()}.
get_subscription_for_k1api_receipt(Receipt = #k_mb_k1api_receipt{}) ->
	MessageID = Receipt#k_mb_k1api_receipt.input_message_id,
	CustomerID = Receipt#k_mb_k1api_receipt.customer_id,
	ClientType = k1api,
	InputID = {CustomerID, ClientType, MessageID},
	?log_debug("InputID: ~p", [InputID]),
	case mnesia:dirty_read(k_mb_k1api_input_id_to_sub_id, InputID) of
		[] ->
			?log_debug("k1api InputID undefined", []),
			undefined;
		[Record] ->
			?log_debug("Record: ~p", [Record]),
			SubID = Record#k_mb_k1api_input_id_to_sub_id.subscription_id,
			?log_debug("SubID: ~p", [SubID]),
			[Sub] = mnesia:dirty_read(k_mb_k1api_receipt_sub, SubID),
			?log_debug("FOUND suitable subscription: ~p", [Sub]),
			{ok, Sub}
	end.

%% @doc Return subscription by id
-spec get_subscription(SubscriptionID :: binary()) ->
	{ok, k_mb_subscription()}.
get_subscription(SubscriptionID) ->
	[Subscription] = mnesia:dirty_read(k_mb_subscription, SubscriptionID),
	{ok, Subscription#k_mb_subscription.value}.

%% @doc Return all subscriptions' ids
-spec get_subscription_ids() -> {ok, [binary()]}.
get_subscription_ids() ->
	IDs = mnesia:dirty_all_keys(k_mb_subscription),
	{ok, IDs}.

%% @doc Delete expired pending items from db
%% -spec delete_expired(Now :: integer()) -> ok.
%% delete_expired(Now) ->
%% 	{atomic, ItemIDs} = mnesia:transaction(fun() ->
%% 		qlc:e(qlc:q([
%% 				Item#k_mb_pending_item.item_id
%% 				|| Item <- mnesia:table(k_mb_pending_item),
%% 				Item#k_mb_pending_item.expire =< Now
%% 		]))
%% 	end),
%% 	lists:foreach(fun delete_item/1, ItemIDs).
	%% delete_item(ItemIDs).

%% @doc Set 'wait_for_sub' item status
-spec set_pending(atom(), binary(), binary(), bitstring()) -> ok.
set_pending(ItemType, ItemID, CustomerID, UserID) ->
	Record = #?PENDING_ITEM{
		id = {ItemType, ItemID},
		owner = {CustomerID, UserID}
	},
	ok = mnesia:dirty_write(Record).

%% @doc Return all pending items that waits for subscription
-spec get_pending(CustomerID :: binary(), UserID :: bitstring()) ->
	{ok, []} | {ok, [{ItemType :: atom(), ItemID :: binary()}]}.
get_pending(CustomerID, UserID) ->
	MatchHead = #?PENDING_ITEM{id = '$1', owner = '$2'},
	Guard = {'=:=', '$2', {{CustomerID, UserID}}},
	Result = '$1',
	Items = mnesia:dirty_select(?PENDING_ITEM, [{MatchHead, [Guard], [Result]}]),
	{ok, Items}.

%% @doc Return all incoming pending sms for k1api sms retrieve request
-spec get_incoming_sms(binary(), bitstring(), addr(), integer() | undefined) ->
	{ok, [#k_mb_incoming_sms{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestinationAddr, Limit) ->
	{atomic, AllItems} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_incoming_sms),
				Item#k_mb_incoming_sms.customer_id == CustomerID andalso
				Item#k_mb_incoming_sms.user_id == UserID andalso
				Item#k_mb_incoming_sms.dest_addr == DestinationAddr
		]))
	end),
	Total = length(AllItems),
	Items = first(AllItems, Limit),
	{ok, Items, Total}.

%% @doc Link k1api input sms id to specific subscription id
-spec link_input_id_to_sub_id(	InputID :: input_sms_id(),
								SubscriptionID :: binary()) -> ok.
link_input_id_to_sub_id(InputID, SubscriptionID) ->
	InputIDToSubID = #k_mb_k1api_input_id_to_sub_id{
		input_id = InputID,
		subscription_id = SubscriptionID
	},
	ok = mnesia:dirty_write(InputIDToSubID).

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
