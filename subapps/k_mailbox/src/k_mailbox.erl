-module(k_mailbox).

-include_lib("k_storage/include/mailbox.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
    register_subscription/1,
    register_sms_req_receipts_subscription/1,
    unregister_subscription/3,
    register_incoming_item/1,
    get_incoming_sms/4,
    process_funnel_down_event/0,
    delete_item/1
]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec register_subscription(Subscription::k_mb_subscription()) -> ok.
register_subscription(Subscription) ->
    k_mb_subscription_mgr:register(Subscription).

-spec register_sms_req_receipts_subscription(Subscription::#k_mb_oneapi_receipt_sub{}) -> ok.
register_sms_req_receipts_subscription(Subscription = #k_mb_oneapi_receipt_sub{}) ->
    k_storage_mailbox:save(Subscription).

-spec unregister_subscription(
    SubscriptionID::binary(),
    CustomerID::customer_id(),
    UserID::user_id()
)-> ok.
unregister_subscription(SubscriptionID, CustomerID, UserID) ->
    k_mb_subscription_mgr:unregister(SubscriptionID, CustomerID, UserID).

-spec register_incoming_item(Item::k_mb_item()) -> ok.
register_incoming_item(Item = #k_mb_oneapi_receipt{}) ->
    case k_storage_mailbox:get_subscription_for_oneapi_receipt(Item) of
        undefined ->
            ?log_debug("Suitable subscription NOT FOUND. "
                "Don't register OneAPI receipt", []),
            ok;
        {ok, #k_mb_oneapi_receipt_sub{}} ->
            k_storage_mailbox:save(Item),
            k_mb_wpool:process_incoming_item(Item)
    end;
register_incoming_item(Item) ->
    k_storage_mailbox:save(Item),
    k_mb_wpool:process_incoming_item(Item).

-spec get_incoming_sms(binary(), binary(), addr(), undefined | integer()) ->
    {ok, [#k_mb_incoming_sms{}], Total::integer()}.
get_incoming_sms(CustomerID, UserID, DestAddr, Limit) ->
    k_storage_mailbox:get_incoming_sms(CustomerID, UserID, DestAddr, Limit).

-spec process_funnel_down_event() -> ok.
process_funnel_down_event() ->
    k_mb_subscription_mgr:process_funnel_down_event().

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item) ->
    k_storage_mailbox:delete_item(Item).
