-ifndef(k_mailbox_application_hrl).
-define(k_mailbox_application_hrl, included).

-include_lib("k_common/include/storages.hrl").

-define(APP, k_mailbox).

-define(INCOMING_SMS, 	k_mb_incoming_sms).
-define(K1API_RECEIPT, 	k_mb_k1api_receipt).
-define(FUNNEL_RECEIPT, k_mb_funnel_receipt).
-define(PENDING_ITEM, 	k_mb_pending_item).

-type utcunix() :: integer(). %% utc unix epoch seconds

%% ===================================================================
%% Pending Items
%% ===================================================================

-record(k_mb_pending_item, {
	id :: {atom(), binary()}, %% {ItemType :: atom(), ItemID :: binary()}
	owner :: {customer_id(), user_id()}
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
	user_id				 :: bitstring(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	received 			 :: utcunix(), %% k1api retrieve sms request
	message_body     	 :: binary(),
	encoding 			 :: incoming_sms_encoding(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: utcunix()
}).

-record(k_mb_k1api_receipt, {
	id					 :: binary(),
	customer_id			 :: binary(),
	user_id				 :: bitstring(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	input_message_id	 :: bitstring(), % format?
	message_state		 :: message_state(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: utcunix()
}).


-record(k_mb_funnel_receipt, {
	id					 :: binary(),
	customer_id			 :: binary(),
	user_id				 :: bitstring(),
	source_addr			 :: addr(),
	dest_addr			 :: addr(),
	input_message_id	 :: bitstring(),
	submit_date			 :: utcunix(),
	done_date			 :: utcunix(),
	message_state		 :: message_state(),

	delivery_attempt = 1 :: integer(),
	created_at			 :: utcunix()
}).

-type k_mb_item() ::
	#k_mb_k1api_receipt{} |
	#k_mb_funnel_receipt{} |
	#k_mb_incoming_sms{}.

%% ===================================================================
%% Subscriptions
%% ===================================================================

-define(K1API_RECEIPT_SUB, k_mb_k1api_receipt_sub).
-define(K1API_INCOMING_SMS_SUB, k_mb_k1api_incoming_sms_sub).
-define(K1API_SMS_REQ_RECEIPTS_SUB, k_mb_k1api_sms_req_receipts_sub).
-define(FUNNEL_SUB, k_mb_funnel_sub).

-record(k_mb_k1api_receipt_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: bitstring(),
	queue_name 		:: bitstring(),
	dest_addr 		:: addr(),		%% rename to source_addr
	notify_url 		:: bitstring(),
	callback_data 	:: bitstring(),
	created_at		:: utcunix()
}).

-record(k_mb_k1api_incoming_sms_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: bitstring(),
	priority 		:: integer(),
	queue_name 		:: bitstring(),
	dest_addr 		:: addr(),
	notify_url 		:: bitstring(),
	criteria 		:: bitstring(),
	callback_data 	:: bitstring(),
	created_at 		:: utcunix()
}).

-record(k_mb_funnel_sub, {
	id 				:: binary(),
	customer_id 	:: binary(),
	user_id 		:: bitstring(),
	priority 		:: integer(),
	queue_name 		:: bitstring(),
	created_at 		:: utcunix()
}).

%% subscriptions for specific k1api sms req messages
-record(k_mb_k1api_input_id_to_sub_id, {
	input_id 		:: binary(),
	subscription_id :: binary()
}).
%%

-type k_mb_subscription() ::
	#k_mb_k1api_receipt_sub{} |
	#k_mb_k1api_incoming_sms_sub{} |
	#k_mb_funnel_sub{}.

%% storage for regular subscriptions
%% (exclude k_mb_k1api_sms_req_receipts_sub)
-record(k_mb_subscription, {
	key				:: binary(),
	value 			:: #k_mb_k1api_receipt_sub{} |
						#k_mb_k1api_incoming_sms_sub{} |
						#k_mb_funnel_sub{}
}).

-endif. % k_mailbox_application_hrl
