-ifndef(provider_hrl).
-define(provider_hrl, included).

-include("storages.hrl").
-include("gateway.hrl").

-type provider_id()       :: uuid().

-record(provider, {
    id                    :: provider_id(),
    name                  :: binary(),
    description           :: binary(),
    gateway_id            :: gateway_id(),
    bulk_gateway_id       :: gateway_id(),
    receipts_supported    :: boolean(),
    sms_add_points = 0.0  :: float()
}).

-type provider()          :: #provider{}.

-endif.
