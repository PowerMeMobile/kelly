-ifndef(network_map_hrl).
-define(network_map_hrl, included).

-include("storages.hrl").
-include("network.hrl").

-record(network_map, {
    name               :: binary(),
    network_ids        :: [network_id()]
}).

-type network_map_id() :: uuid().
-type network_map()    :: #network_map{}.

-endif.
