-ifndef(k_mailbox_address_hrl).
-define(k_mailbox_address_hrl, included).

-include_lib("k_common/include/customer.hrl").

-record(k_mb_address, {
	address		:: addr(),
	customer_id :: customer_id(),
	user_id		:: user_id()
}).

-endif.
