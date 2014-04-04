-module(k_amqp_req).

-include("amqp_req.hrl").

-type req() :: #amqp_req{}.
-export_type([req/0]).

-export([
    payload/1,
    content_type/1,
    channel/1,
    reply_to/1
]).

-spec payload(Req :: #amqp_req{}) -> {ok, Payload :: binary()}.
payload(Req = #amqp_req{}) ->
    {ok, Req#amqp_req.'req.payload'}.

-spec content_type(Req :: #amqp_req{}) -> {ok, ContentType :: binary()}.
content_type(Req = #amqp_req{}) ->
    {ok, Req#amqp_req.'req.p_basic.content_type'}.

-spec channel(Req :: #amqp_req{}) -> {ok, Chan :: pid()}.
channel(Req = #amqp_req{}) ->
    {ok, Req#amqp_req.'amqp.channel'}.

-spec reply_to(Req :: #amqp_req{}) -> {ok, QueueName :: binary()}.
reply_to(Req = #amqp_req{}) ->
    {ok, Req#amqp_req.'req.p_basic.reply_to'}.
