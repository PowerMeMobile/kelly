-ifndef(k_mailbox_application_hrl).
-define(k_mailbox_application_hrl, included).

-include_lib("k_storage/include/customer.hrl").

-define(APP, k_mailbox).
-define(msisdnsColl, msisdns).
-define(funnelReceiptsColl, mb_funnel_receipts).
-define(incomingSmsColl, mb_incoming_sms).
-define(inputIdToSubIdColl, mb_k1api_input_id_to_sub_id).
-define(k1apiReceiptsColl, mb_k1api_receipts).
-define(k1apiReceiptSubColl, mb_k1api_receipt_subs).
-define(pendingItemsColl, mb_pending_items).
-define(subscriptionsColl, mb_subscriptions).

%% ===================================================================
%% Pending Items
%% ===================================================================

-record(k_mb_pending_item, {
	id			:: binary(),
	type		:: atom(),
	customer_id	:: binary(),
	user_id 	:: binary()
}).

%% ===================================================================
%% Items
%% ===================================================================

-type item_id() :: binary().

-type incoming_sms_encoding() ::
	default |
	gsm033 |
	ascii |
	latin1 |
	ucs2 |
	integer().

-type message_state() ::
	enroute |
	delivered |
    expired |
    deleted |
    undeliverable |
    accepted |
    unknown |
    rejected |
    unrecognized.

-record(k_mb_incoming_sms, {
	id					 :: binary(),
	customer_id			 :: binary(),
	user_id				 :: binary(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	received 			 :: erlang:timestamp(), %% k1api retrieve sms request
	message_body     	 :: binary(),
	encoding 			 :: incoming_sms_encoding(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: erlang:timestamp()
}).

-record(k_mb_k1api_receipt, {
	id					 :: binary(),
	customer_id			 :: binary(),
	user_id				 :: binary(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	input_message_id	 :: binary(), % format?
	message_state		 :: message_state(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: erlang:timestamp()
}).


-record(k_mb_funnel_receipt, {
	id					 :: binary(),
	customer_id			 :: binary(),
	user_id				 :: binary(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	input_message_id	 :: binary(),
	submit_date			 :: erlang:timestamp(),
	done_date			 :: erlang:timestamp(),
	message_state		 :: message_state(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: erlang:timestamp()
}).

-type k_mb_item() ::
	#k_mb_k1api_receipt{} |
	#k_mb_funnel_receipt{} |
	#k_mb_incoming_sms{}.

%% ===================================================================
%% Subscriptions
%% ===================================================================

-record(k_mb_k1api_receipt_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: binary(),
	queue_name 		:: binary(),
	dest_addr 		:: addr(),		%% rename to source_addr
	notify_url 		:: binary(),
	callback_data 	:: binary(),
	created_at		:: erlang:timestamp()
}).

-record(k_mb_k1api_incoming_sms_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: binary(),
	priority 		:: integer(),
	queue_name 		:: binary(),
	dest_addr 		:: addr(),
	notify_url 		:: binary(),
	criteria 		:: binary(),
	callback_data 	:: binary(),
	created_at 		:: erlang:timestamp()
}).

-record(k_mb_funnel_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: binary(),
	priority 		:: integer(),
	queue_name 		:: binary(),
	created_at 		:: erlang:timestamp()
}).

-type k_mb_subscription() ::
	#k_mb_k1api_receipt_sub{} |
	#k_mb_k1api_incoming_sms_sub{} |
	#k_mb_funnel_sub{}.

-endif. % k_mailbox_application_hrl
