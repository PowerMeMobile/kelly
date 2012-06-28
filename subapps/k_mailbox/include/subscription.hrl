-ifndef(k_mb_subscription_hrl).
-define(k_mb_subscription_hrl, included).

-include_lib("k_common/include/storages.hrl").

-type subscription_type() ::
	receiver |
	transceiver |
	dlvrReceiptReceiver |
	incomingSMSReceiver.

-type subscription_id() :: string().

-record(k_mb_subscription, {
	id :: subscription_id(),
	customer_id :: customer_id(),
	user_id :: user_id(),
	type :: subscription_type(),
	priority = 0 :: integer(),
	app_type :: smpp | oneapi,
	queue_name :: binary(),

	notify_url :: string(),
	criteria :: string(),
	callback_data :: string(),
	correlator :: string(),
	notification_format :: string()
}).

-endif. % k_mb_subscription_hrl
