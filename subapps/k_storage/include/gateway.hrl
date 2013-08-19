-ifndef(gateway_hrl).
-define(gateway_hrl, included).

-include("storages.hrl").

-type connection_id() 	:: integer().
-type smpp_bind_type() :: transmitter | receiver | transceiver.

-record(connection, {
	id 					:: connection_id(),
	host 				:: binary(),
	port 				:: integer(),
	bind_type			:: smpp_bind_type(),
	system_id			:: binary(),
	password 			:: binary(),
	system_type			:: binary(),
	addr_ton 			:: integer(),
	addr_npi 			:: integer(),
	addr_range 			:: binary()
}).

-type connection() 		:: #connection{}.

-record(setting, {
	name				:: binary(),
	value				:: binary()
}).
-type setting() 		:: #setting{}.

-record(gateway, {
	name 			 	:: binary(),
	rps 			 	:: integer(),
	connections = [] 	:: [connection()],
	settings = []		:: [setting()]

}).

-type gateway() 		:: #gateway{}.
-type gateway_id() 		:: uuid().

-endif.
