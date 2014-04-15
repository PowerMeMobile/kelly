-ifndef(network_hrl).
-define(network_hrl, included).

-include("storages.hrl").
-include("provider.hrl").

-record(network, {
    name                  :: binary(),
    country               :: binary(),
    hex_code              :: binary(),
    country_code          :: binary(),
    number_len = 0        :: pos_integer(),
    prefixes = []         :: [binary()],
    gmt_diff              :: binary(),
    dst                   :: binary(),
    provider_id           :: provider_id(),
    is_home = false       :: boolean(),
    sms_points = 0.0      :: float(),
    sms_mult_points = 1.0 :: float()
}).

-type network_id()        :: uuid().
-type network()           :: #network{}.

-endif.
