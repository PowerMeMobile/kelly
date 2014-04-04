-ifndef(manifest_hrl).
-define(manifest_hrl, included).

-include ("daily_cfg.hrl").

-type date_tuple() :: {integer(), integer(), integer()}.

-record(manifest, {
    last_rotated :: date_tuple(),
    partitions = [] :: [#daily_cfg{}]
}).

-endif. % manifest_hrl
