-module(k_null_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, _Message) ->
	{ok, []}.
