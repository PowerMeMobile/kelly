-ifndef(amqp_req_hrl).
-define(amqp_req_hrl, included).

-record(amqp_req, {
	'req.payload' :: binary(),
	'req.p_basic.content_type' :: binary(),
	'req.p_basic.reply_to' :: binary(),
	'amqp.channel' :: pid()
}).

-endif. % amqp_req_hrl
