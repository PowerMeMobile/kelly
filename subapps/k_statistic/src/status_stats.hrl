-ifndef(status_stats_hrl).
-define(status_stats_hrl, included).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").

-record(status_stats, {
	input_id  :: msg_id(),
	output_id :: msg_id(),
	msg_info  :: #msg_info{},
	msg_status :: #msg_status{},
	time  :: integer()
}).

-endif.
