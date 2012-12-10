-ifndef(storages_hrl).
-define(storages_hrl, included).

-define(networkStorageName, networks).

-type ver() 			:: integer().
-type error() 			:: term().
-type uuid() 			:: binary().

-record(addr, {
	addr 				:: binary(),
	ton 				:: integer(),
	npi 				:: integer()
}).
-type addr() 			:: #addr{}.

-endif.
