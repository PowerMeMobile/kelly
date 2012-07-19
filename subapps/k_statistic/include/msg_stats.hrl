-ifndef(msg_stats_hrl).
-define(msg_stats_hrl, included).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").

-record(msg_stats, {
	id  :: msg_id(),
	msg_info  :: #msg_info{},
	time  :: integer()
}).

-endif.
