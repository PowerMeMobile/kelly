-ifndef(k_mb_pending_item_hrl).
-define(k_mb_pending_item_hrl, included).

-include_lib("k_common/include/storages.hrl").

-type content_type() :: delivery_receipt | incoming_sms.
-type encoding() ::
	{text, default} |
	{text, gsm0338} |
	{text, ascii} |
	{text, latin1} |
	{text, ucs2} |
	{other, integer()}.


-record(k_mb_pending_item, {
	item_id :: binary(),
	customer_id :: binary(),
	user_id :: bitstring(),
	content_type :: content_type(),
	sender_addr :: addr(), %% #addr{}
	dest_addr :: addr(), %% #addr{}
	timestamp :: integer(), %% utc unix epoch seconds
	message_body :: bitstring(),
	content_body :: binary(), %% funnel batch
	encoding :: encoding(),
	attempt = 1 :: integer(), % delivery attempt counter
	error :: term(), % last error term
	state = pending :: 	pending |
						wait_for_sub,
	expire :: integer() % time in seconds when item will become expired
}).

-endif. % k_mb_pending_item_hrl
