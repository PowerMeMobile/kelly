-module(k_statistic_uplink_stats_report_helper).

-export([
    get_gtws_throughput/0
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/JustAsn.hrl").

-spec get_gtws_throughput() -> ok.
get_gtws_throughput() ->
    {ok, Chan} = rmql:channel_open(),
    {ok, Payload} = 'JustAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
    {ok, TmpQueue} = queue_declare(Chan),
    {ok, _ConsumerTag} = rmql:basic_consume(Chan, TmpQueue, true),
    BProps = [{reply_to, TmpQueue}, {message_id, <<"1">>}, {content_type, <<"ThroughputRequest">>}],
    {ok, QName} = application:get_env(k_handlers, just_control_queue),
    ok = rmql:basic_publish(Chan, QName, Payload, BProps),
    Response =
        receive
            {#'basic.deliver'{}, #amqp_msg{payload = RespPayload, props = RespProps}} ->
                process_response(RespPayload, RespProps)
        after
            10000 ->
                {error, timeout}
        end,
    rmql:channel_close(Chan),
    Response.

process_response(Payload, #'P_basic'{content_type = <<"ThroughputResponse">>}) ->
    'JustAsn':decode('ThroughputResponse', Payload).

queue_declare(Chan) ->
    Method = #'queue.declare'{durable = false,
                              exclusive = true,
                              auto_delete = true},
    try amqp_channel:call(Chan, Method) of
        #'queue.declare_ok'{queue = Queue} -> {ok, Queue};
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.
