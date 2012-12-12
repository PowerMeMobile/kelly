-ifndef(storages_hrl).
-define(storages_hrl, included).

-include_lib("alley_dto/include/addr.hrl").

-define(networkStorageName, networks).
-define(providerStorageName, providers).
-define(gatewayStorageName, gateways).
-define(customerStorageName, customers).

-type error()	:: term().
-type uuid() 	:: binary().

-endif.
