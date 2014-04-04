-ifndef(amqp_worker_reply).
-define(amqp_worker_reply, included).

-record(worker_reply, {
    reply_to     :: binary(),
    payload      :: binary(),
    content_type :: binary()
}).

-endif. % amqp_worker_reply_hrl
