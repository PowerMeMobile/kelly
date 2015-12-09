-ifndef(dealer_hrl).
-define(dealer_hrl, included).

-include("storages.hrl").
-include("network_map.hrl").
-include("provider.hrl").
-include("feature.hrl").

-type dealer_uuid() :: uuid().
-define(ACTIVE_ST, active).
-define(BLOCKED_ST, blocked).
-define(DEACTIVATED_ST, deactivated).
-define(DELETED_ST, deleted).
-type entity_state() ::
    ?ACTIVE_ST | ?BLOCKED_ST | ?DEACTIVATED_ST | ?DELETED_ST.
-type dealer_state() :: entity_state().

-record(dealer_v1, {
    id                  :: dealer_uuid(),
    name                :: binary(),
    network_map_id      :: network_map_id(),
    default_provider_id :: provider_id() | undefined,
    features       = [] :: features(),
    interfaces     = [] :: [interface()],
    state               :: dealer_state()
}).

-type dealer() :: #dealer_v1{}.

-endif.
