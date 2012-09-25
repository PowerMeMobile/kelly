%% @doc Kelly Mailbox interface module.
%% Provides public methods for interaction with mailbox application.

%% @TODO implement removing invalid subscriptions

-module(k_mailbox).

-include("pending_item.hrl").
-include("subscription.hrl").

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	register_subscription/1,
	unregister_subscription/3,
	register_incoming_item/1,
	get_incoming_sms/4
]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

%% @doc Register new subscription
-spec register_subscription(Subscription :: #k_mb_subscription{}) -> ok.
register_subscription(Subscription = #k_mb_subscription{}) ->
	k_mb_db:save(Subscription),
	k_mb_map_mgr:register_subscription(Subscription),
	ok.

%% @doc Unregister subscription
-spec unregister_subscription(SubscriptionID :: string(), CustomerID :: string(),
	UserID :: string()) -> ok.
unregister_subscription(SubscriptionID, CustomerID, UserID) ->
	k_mb_db:delete_subscriptions([SubscriptionID]),
	k_mb_map_mgr:unregister_subscription(CustomerID, UserID, SubscriptionID),
	ok.

%% @doc Register incoming message or receipt
-spec register_incoming_item(#k_mb_pending_item{}) -> ok.
register_incoming_item(Item = #k_mb_pending_item{}) ->
	Expire = k_mb_gcollector:new_expire(),
	save_pending_item(Item#k_mb_pending_item{expire = Expire}).

%% @doc Give incoming sms (pending items) from db. Used by k1api handler
-spec get_incoming_sms(binary(), bitstring(), addr(), undefined | integer()) ->
	{ok, [#k_mb_pending_item{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestAddr, Limit) ->
	k_mb_db:get_incoming_sms(CustomerID, UserID, DestAddr, Limit).

%% ===================================================================
%% Internal
%% ===================================================================

save_pending_item(Item) ->
	k_mb_db:save(Item), % add gproc:lookup(k_mb_wpool) before?
	k_mb_wpool:process_incoming_item(Item),
	ok.
