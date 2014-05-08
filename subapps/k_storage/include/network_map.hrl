-ifndef(network_map_hrl).
-define(network_map_hrl, included).

-include("storages.hrl").
-include("network.hrl").

-type network_map_id() :: uuid().

-record(network_map, {
    id                 :: network_map_id(),
    name               :: binary(),
    network_ids        :: [network_id()]
}).

-type network_map()    :: #network_map{}.

-endif.
