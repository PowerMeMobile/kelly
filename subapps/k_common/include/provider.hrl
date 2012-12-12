-ifndef(provider_hrl).
-define(provider_hrl, included).

-include("storages.hrl").
-include("gateway.hrl").

-record(provider, {
	name 			  	:: binary(),
	gateway 		  	:: gateway_id(),
	bulk_gateway 	  	:: gateway_id(),
	receipts_supported 	:: boolean()
}).
-type provider_id() 	:: uuid().
-type provider() 		:: #provider{}.

-endif.
