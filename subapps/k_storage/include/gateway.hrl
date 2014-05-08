-ifndef(gateway_hrl).
-define(gateway_hrl, included).

-include("storages.hrl").

-type gateway_id()      :: uuid().
-type connection_id()   :: integer().
-type smpp_bind_type() :: transmitter | receiver | transceiver.

-record(connection, {
    id                  :: connection_id(),
    host                :: binary(),
    port                :: integer(),
    bind_type           :: smpp_bind_type(),
    system_id           :: binary(),
    password            :: binary(),
    system_type         :: binary(),
    addr_ton            :: integer(),
    addr_npi            :: integer(),
    addr_range          :: binary()
}).

-record(setting, {
    name                :: binary(),
    value               :: binary()
}).

-record(gateway, {
    id                  :: gateway_id(),
    name                :: binary(),
    rps                 :: integer(),
    connections = []    :: [connection()],
    settings = []       :: [setting()]

}).

-type connection()      :: #connection{}.
-type setting()         :: #setting{}.
-type gateway()         :: #gateway{}.

-endif.
