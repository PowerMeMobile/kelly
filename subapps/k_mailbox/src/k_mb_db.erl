%% @private
%% @doc This module is rersponsible for all database operations.

%% @TODO get_wait/2 => add content_type restriction

-module(k_mb_db).

-include_lib("k_common/include/logging.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("subscription.hrl").
-include("pending_item.hrl").
-include("address.hrl").

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	init/0,
	save/1,
	delete_expired/1,
	get_customers/0,

	get_subscription/1,
	get_subscriptions/2,
	delete_subscriptions/1,

	get_items/0,
	get_pending_item/1,
	delete_items/1,

	set_wait/1,
	get_wait/2,
	set_pending/1,

	get_incoming_sms/4,
	get_all/0
]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

%% @doc Sure in table existance & creates it if neccessary.
%% Used only at application initialization step.
-spec init() -> ok.
init() ->
	ok = k_mnesia_schema:ensure_table(
		k_mb_subscription,
		record_info(fields, k_mb_subscription)),
	ok = k_mnesia_schema:ensure_table(
		k_mb_pending_item,
		record_info(fields, k_mb_pending_item)),
	ok = k_mnesia_schema:ensure_table(
		k_mb_address,
		record_info(fields, k_mb_address)).

%% @doc Save known entries in database.
%% Known are: k_mb_pending_item, k_mb_subscription, k_mb_address
-spec save(Record :: tuple()) -> ok.
save(What) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		ok = mnesia:write(What)
	end),
	ok.

%% @doc Delete subscriptions from DB.
-spec delete_subscriptions(SubscriptionIDs :: list(string())) -> ok.
delete_subscriptions(SubscriptionIDs) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		lists:foreach(fun(SubscriptionID) ->
			mnesia:delete({k_mb_subscription, SubscriptionID})
		end,
		SubscriptionIDs)
	end),
	ok.

-spec delete_items(ItemIDsList :: list(string())) -> ok.
delete_items(ItemIDsList) ->
		{atomic, ok} = mnesia:transaction(fun() ->
		lists:foreach(fun(ItemID) ->
			mnesia:delete({k_mb_pending_item, ItemID})
		end,
		ItemIDsList)
	end),
	ok.

-spec get_items() -> {ok, []} | {ok, [#k_mb_pending_item{}]}.
get_items() ->
	{atomic, Items} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_pending_item)
		]))
	end),
	{ok, Items}.


-spec get_pending_item(ItemID :: string()) -> Item :: #k_mb_pending_item{}.
get_pending_item(ItemID) ->
	{atomic, [Item]} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_pending_item),
				Item#k_mb_pending_item.item_id == ItemID
		]))
	end),
	{ok, Item}.

-spec get_subscription(SubscriptionID :: string()) -> {ok, #k_mb_subscription{}}.
get_subscription(SubscriptionID) ->
	{atomic, Subscription} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Sub || Sub <- mnesia:table(k_mb_subscription),
				Sub#k_mb_subscription.id == SubscriptionID
		]))
	end),
	{ok, Subscription}.

-spec get_subscriptions(CustomerID :: string(), UserID :: string()) -> {ok, [#k_mb_subscription{}]} | {ok, []}.
get_subscriptions(CustomerID, UserID) ->
	{atomic, Subscriptions} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Sub || Sub <- mnesia:table(k_mb_subscription),
				Sub#k_mb_subscription.customer_id == CustomerID
				andalso
				Sub#k_mb_subscription.user_id == UserID
		]))
	end),
	{ok, Subscriptions}.

-spec get_customers() -> {ok, []} | {ok, [{}]}.
get_customers() ->
		{atomic, Customers} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				{Sub#k_mb_subscription.customer_id, Sub#k_mb_subscription.user_id}
				|| Sub <- mnesia:table(k_mb_subscription)],
				{unique, true}
		))
	end),
	{ok, Customers}.

-spec delete_expired(Now :: integer()) -> ok.
delete_expired(Now) ->
	{atomic, ItemIDs} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item#k_mb_pending_item.item_id
				|| Item <- mnesia:table(k_mb_pending_item),
				Item#k_mb_pending_item.expire =< Now
		]))
	end),
	delete_items(ItemIDs).

-spec set_wait(ItemID :: string()) -> ok.
set_wait(ItemID) ->
	change_item_status(ItemID, wait_for_sub).

-spec set_pending(ItemID :: string()) -> ok.
 set_pending(ItemID) ->
	change_item_status(ItemID, pending).

change_item_status(ItemID, Status) ->
	{ok, Item} = get_pending_item(ItemID),
	save(Item#k_mb_pending_item{state = Status}).

-spec get_wait(CustomerID :: string(), UserID :: string()) ->
	{ok, []} | {ok, [#k_mb_pending_item{}]}.
get_wait(CustomerID, UserID) ->
	{atomic, Items} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_pending_item),
				Item#k_mb_pending_item.customer_id == CustomerID
				andalso
				Item#k_mb_pending_item.user_id == UserID
				andalso
				Item#k_mb_pending_item.state == wait_for_sub
		]))
	end),
	{ok, Items}.

-spec get_incoming_sms(binary(), bitstring(), addr(), integer() | undefined) ->
	{ok, [#k_mb_pending_item{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestinationAddr, Limit) ->
	{atomic, AllItems} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_pending_item),
				Item#k_mb_pending_item.customer_id == CustomerID andalso
				Item#k_mb_pending_item.user_id == UserID andalso
				Item#k_mb_pending_item.dest_addr == DestinationAddr
		]))
	end),

	Total = length(AllItems),
	Items = first(AllItems, Limit),
	{ok, Items, Total}.

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

%% temp
get_all() ->
	{atomic, Items} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item || Item <- mnesia:table(k_mb_pending_item)
		], {max_lookup, 2}))
	end),
	first(Items, 4).
