-ifndef(blacklist_entry_hrl).
-define(blacklist_entry_hrl, included).

-include("storages.hrl").

-type blacklist_entry_id() :: uuid().

-record(blacklist_entry, {
    id       :: blacklist_entry_id(),
    dst_addr :: #addr{},
    src_addr :: #addr{}
}).

-type blacklist_entry()    :: #blacklist_entry{}.

-endif.
