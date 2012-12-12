-ifndef(gateway_hrl).
-define(gateway_hrl, included).

-include("storages.hrl").

-type connection_id() 	:: integer().
-record(connection, {
	id 					:: connection_id(),
	type 				:: integer(),
	addr 				:: binary(),
	port 				:: integer(),
	sys_id 				:: binary(),
	pass 				:: binary(),
	sys_type 			:: binary(),
	addr_ton 			:: integer(),
	addr_npi 			:: integer(),
	addr_range 			:: binary()
}).
-type connection() 		:: #connection{}.
-record(gateway, {
	name 			 	:: binary(),
	rps 			 	:: integer(),
	connections = [] 	:: [connection()] | []
}).
-type gateway_id() 		:: uuid().
-type gateway() 		:: #gateway{}.

-endif.
