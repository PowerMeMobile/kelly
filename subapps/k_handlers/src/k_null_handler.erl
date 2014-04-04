-module(k_null_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_Req) ->
    {ok, []}.
