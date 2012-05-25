-ifndef(k_mb_pending_item_hrl).
-define(k_mb_pending_item_hrl, included).

-record(k_mb_pending_item, {
	item_id :: string(),
	customer_id :: string(),
	content_type :: binary(),
	content_body :: binary(),
	expire :: integer(),
	attempt = 1 :: integer(),
	error :: term(),
	state = pending :: pending | wait_for_sub
}).

-endif. % k_mb_pending_item_hrl
