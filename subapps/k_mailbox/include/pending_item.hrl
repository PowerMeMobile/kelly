-ifndef(k_mb_pending_item_hrl).
-define(k_mb_pending_item_hrl, included).

-type content_type() :: binary(). 	%% <<"OutgoingBatch">> |
									%% <<"ReceiptBatch">>.

-record(k_mb_pending_item, {
	item_id :: string(),
	customer_id :: string(),
	user_id :: string(),
	content_type :: content_type(),
	content_body :: binary(),
	attempt = 1 :: integer(), % attempt counter
	error :: term(), % last error term
	state = pending :: 	pending |
						wait_for_sub,
	expire :: integer() % time in seconds when item will become expired
}).

-endif. % k_mb_pending_item_hrl