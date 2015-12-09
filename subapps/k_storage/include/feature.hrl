-ifndef(feature_hrl).
-define(feature_hrl, included).

-record(feature, {
    name :: binary(),
    value :: binary()
}).

-type feature() :: #feature{}.
-type features() :: [feature()].

-endif.
