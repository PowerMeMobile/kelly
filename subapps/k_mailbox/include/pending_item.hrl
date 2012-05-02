-ifndef(k_mb_pending_item_hrl).
-define(k_mb_pending_item_hrl, included).

-record(k_mb_pending_item, {
	item_id,
	customer_id,
	content_type,
	content_body,
	state = pending :: pending | submitted | {failed, any(), any()} | {failed, any()}
}).

-endif. % k_mb_pending_item_hrl
