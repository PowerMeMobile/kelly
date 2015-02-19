-module(k_j3_support_rmq).

-export([
    rpc_call/3
]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec rpc_call(binary(), binary(), binary()) ->
    {ok, binary(), binary()} | {error, term()}.
rpc_call(CtrlQueue, ReqCT, ReqPayload) ->
    {ok, Chan} = rmql:channel_open(),
    {ok, ReplyToQueue} = reply_to_queue_declare(Chan),
    {ok, _ConsumerTag} = rmql:basic_consume(Chan, ReplyToQueue, true),
    MsgId = uuid:unparse(uuid:generate()),
    Props = [
        {reply_to, ReplyToQueue},
        {message_id, MsgId},
        {content_type, ReqCT}
    ],
    ok = rmql:basic_publish(Chan, CtrlQueue, ReqPayload, Props),
    Response =
        receive
            {#'basic.deliver'{}, #amqp_msg{
                payload = RespPayload,
                props = #'P_basic'{
                    correlation_id = MsgId,
                    content_type = RespCT
                }
            }} ->
                {ok, RespCT, RespPayload}
        after
            10000 ->
                {error, timeout}
        end,
    rmql:channel_close(Chan),
    Response.

%% ===================================================================
%% Internal
%% ===================================================================

reply_to_queue_declare(Chan) ->
    Method = #'queue.declare'{
        durable = false,
        exclusive = true,
        auto_delete = true
    },
    try amqp_channel:call(Chan, Method) of
        #'queue.declare_ok'{queue = Queue} ->
            {ok, Queue};
        Other ->
            {error, Other}
    catch
        _:Reason ->
            {error, Reason}
    end.
