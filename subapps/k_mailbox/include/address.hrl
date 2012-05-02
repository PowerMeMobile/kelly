-ifndef(k_mailbox_address_hrl).
-define(k_mailbox_address_hrl, included).

-type address() :: {
	Addr :: string(),
	Ton :: integer(),
	Npi :: integer()
}.

-record(k_mb_address, {
	address :: address(),
	customer_id :: string()
}).

-endif.
