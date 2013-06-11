-module(k_statistic_uplink_stats_report_helper).

-export([
	get_gtws_throughput/0
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/JustAsn.hrl").

-define(CONTROL_KEY, <<"pmm.just.control">>).

-spec get_gtws_throughput() -> ok.
get_gtws_throughput() ->
	{ok, Conn} = rmql:connection_start(),
	link(Conn),
	{ok, Chan} = rmql:channel_open(Conn),
    {ok, Payload} = 'JustAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
	{ok, TmpQueue} = queue_declare(Chan),
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, TmpQueue, true),
	BProps = [{reply_to, TmpQueue}, {message_id, <<"1">>}, {content_type, <<"ThroughputRequest">>}],
	ok = rmql:basic_publish(Chan, ?CONTROL_KEY, list_to_binary(Payload), BProps),
	Response =
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = RespPayload, props = RespProps}} ->
			process_response(RespPayload, RespProps)
	after
		10000 ->
			{error, timeout}
	end,
	unlink(Conn),
	rmql:connection_close(Conn),
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
