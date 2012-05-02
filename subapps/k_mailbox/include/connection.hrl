-ifndef(k_mb_connection_hrl).
-define(k_mb_connection_hrl, included).

-type connection_type() :: 
	'smpp.receiver' |
	'smpp.transceiver' | 
	'smpp.transmitter'.

-type content_type() :: binary().

-type connection_id() :: string().

-type customer_id() :: string().

-record(k_mb_connection, {
	connection_id :: connection_id(),
	customer_id :: customer_id(),
	connection_type :: connection_type()
}).

-endif. % k_mb_connection_hrl
