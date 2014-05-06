-ifndef(blacklist_entry_hrl).
-define(blacklist_entry_hrl, included).

-include("storages.hrl").

-record(blacklist_entry, {
    dst_addr :: #addr{},
    src_addr :: #addr{}
}).

-type blacklist_entry()    :: #blacklist_entry{}.
-type blacklist_entry_id() :: uuid().

-endif.
