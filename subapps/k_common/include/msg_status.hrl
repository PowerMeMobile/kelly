-ifndef(msg_status_hrl).
-define(msg_status_hrl, included).

-type status() :: received
				| submitted
				| success
				| failure
				| accepted
				| deleted
				| delivered
				| expired
				| rejected
				| undeliverable
				| unknown.

-type unix_epoch() :: integer().

-record(msg_status, {
	status        :: status(),
	req_time = 0  :: unix_epoch(),
	resp_time = 0 :: unix_epoch(),
	dlr_time = 0  :: unix_epoch()
}).

-endif. % msg_status_hrl
