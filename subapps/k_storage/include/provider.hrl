-ifndef(provider_hrl).
-define(provider_hrl, included).

-include("storages.hrl").
-include("gateway.hrl").

-record(provider, {
    name                  :: binary(),
    gateway_id            :: gateway_id(),
    bulk_gateway_id       :: gateway_id(),
    receipts_supported    :: boolean(),
    sms_add_credits = 0.0 :: float()
}).
-type provider_id()     :: uuid().
-type provider()        :: #provider{}.

-endif.
