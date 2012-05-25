-ifndef(k_mb_subscription_hrl).
-define(k_mb_subscription_hrl, included).

-type sub_type() :: 
	'smpp.receiver' |
	'smpp.transceiver' | 
	'smpp.transmitter'.

-record(k_mb_subscription, {
	id :: string(),
	customer_id :: string(),
	sub_type :: binary()
}).

-endif. % k_mb_subscription_hrl