-module(k_mailbox).

-export([
	register_connection/3,
	unregister_connection/2,
	register_incoming_item/4
]).

-include("pending_item.hrl").
-include("subscription.hrl").

-spec register_connection(CustomerID :: string(), ConnID :: string(), ConnType :: atom()) -> ok.
register_connection(CustomerID, ConnID, ConnType) ->
	Subscription = #k_mb_subscription{
		id = ConnID,
		customer_id = CustomerID,
		sub_type = ConnType
		},
	k_mb_db:save(Subscription),
	k_mb_map_mgr:register_subscription(Subscription).

-spec unregister_connection(CustomerID :: string(), ConnID :: string()) -> ok.
unregister_connection(CustID, ConnID) ->
	k_mb_db:delete_subscriptions([ConnID]),
	k_mb_map_mgr:unregister_subscription(CustID, ConnID). %% !!!! removed UserID

-spec register_incoming_item(ItemID :: string(), CustomerID :: string(), ContentType :: binary() , ContentBody :: binary()) -> ok.
register_incoming_item(ItemID, CustID, ContentType, ContentBody) ->
	Expire = k_mb_gcollector:new_expire(),
	Item = #k_mb_pending_item{
				item_id = ItemID,
				customer_id = CustID,
				content_type = ContentType,
				content_body = ContentBody,
				expire = Expire
			},
	k_mb_db:save(Item),
	k_mb_wpool:process_incoming_item(Item).
