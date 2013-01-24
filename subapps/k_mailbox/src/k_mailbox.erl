%% @TODO Removing expired items & k1api receipt subscriptions
%% @TODO Removing successfully retrieved to k1api sms messages
-module(k_mailbox).

-include("application.hrl").

-type input_sms_id() :: {	CustomerID :: binary(),
							ClientType :: atom(),
							MessageID :: bitstring()
						}.

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	register_subscription/1,
	register_sms_req_receipts_subscription/1,
	unregister_subscription/3,
	register_incoming_item/1,
	get_incoming_sms/4
]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec register_subscription(Subscription :: k_mb_subscription()) -> ok.
register_subscription(Subscription) ->
	k_mb_subscription_mgr:register(Subscription).

-spec register_sms_req_receipts_subscription(Subscription :: #k_mb_k1api_receipt_sub{}) -> ok.
register_sms_req_receipts_subscription(Subscription = #k_mb_k1api_receipt_sub{}) ->
	k_mb_db:save(Subscription).

-spec unregister_subscription(	SubscriptionID :: binary(),
								CustomerID :: customer_id(),
								UserID :: user_id()	) -> ok.
unregister_subscription(SubscriptionID, CustomerID, UserID) ->
	k_mb_subscription_mgr:unregister(SubscriptionID, CustomerID, UserID).

-spec register_incoming_item(Item :: k_mb_item()) -> ok.
register_incoming_item(Item) ->
	%% Expire = k_mb_gcollector:new_expire(),
	k_mb_db:save(Item),
	k_mb_wpool:process_incoming_item(Item).

-spec get_incoming_sms(binary(), bitstring(), addr(), undefined | integer()) ->
	{ok, [#k_mb_incoming_sms{}], Total :: integer()}.
get_incoming_sms(CustomerID, UserID, DestAddr, Limit) ->
	k_mb_db:get_incoming_sms(CustomerID, UserID, DestAddr, Limit).

%% ===================================================================
%% Internal
%% ===================================================================
