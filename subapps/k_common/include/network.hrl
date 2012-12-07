-ifndef(network_hrl).
-define(network_hrl, included).

-include("storages.hrl").
-include("provider.hrl").

-record(network, {
	name 				:: binary(),
	country_code 		:: binary(),
	numbers_len			:: integer(),
	prefixes 			:: [binary()],
	provider_id			:: provider_id()
}).
-type network_id() 		:: uuid().
-type network() 		:: {ver(), #network{}}.

-endif.
