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
    get_incoming/4,
    process_funnel_down_event/0,
    delete_item/1
]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec register_subscription(k_mb_subscription()) -> ok.
register_subscription(Sub) ->
    k_mb_subscription_mgr:register(Sub).

-spec register_sms_req_receipts_subscription(#k_mb_oneapi_receipt_sub{}) -> ok.
register_sms_req_receipts_subscription(Sub = #k_mb_oneapi_receipt_sub{}) ->
    k_storage_mailbox:save(Sub).

-spec unregister_subscription(uuid(), customer_uuid(), user_id()) -> ok.
unregister_subscription(SubId, CustomerUuid, UserId) ->
    k_mb_subscription_mgr:unregister(SubId, CustomerUuid, UserId).

-spec register_incoming_item(Item::k_mb_item()) -> ok.
register_incoming_item(Item = #k_mb_oneapi_receipt{}) ->
    case k_storage_mailbox:get_subscription_for_oneapi_receipt(Item) of
        undefined ->
            ?log_debug("Suitable subscription NOT FOUND"
                "Don't register OneAPI receipt", []),
            ok;
        {ok, #k_mb_oneapi_receipt_sub{}} ->
            k_storage_mailbox:save(Item),
            k_mb_wpool:process_incoming_item(Item)
    end;
register_incoming_item(Item) ->
    k_storage_mailbox:save(Item),
    k_mb_wpool:process_incoming_item(Item).

-spec get_incoming(customer_uuid(), user_id(), addr(), undefined | integer()) ->
    {ok, [#k_mb_incoming{}], Total::integer()}.
get_incoming(CustomerUuid, UserId, DestAddr, Limit) ->
    k_storage_mailbox:get_incoming(CustomerUuid, UserId, DestAddr, Limit).

-spec process_funnel_down_event() -> ok.
process_funnel_down_event() ->
    k_mb_subscription_mgr:process_funnel_down_event().

-spec delete_item(k_mb_item()) -> ok.
delete_item(Item) ->
    k_storage_mailbox:delete_item(Item).
