-module(k_mb_amqp_funs).

-compile([{parse_transform, lager_transform}]).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([connection_start/0, connection_close/1]).
-export([channel_open/1, channel_close/1]).
-export([queue_declare/3, queue_declare/5]).
-export([basic_qos/2, basic_consume/3, basic_cancel/2]).
-export([basic_publish/4, basic_ack/2, basic_reject/3]).
-export([tx_select/1, tx_commit/1]).

%% -------------------------------------------------------------------------
%% Connection methods
%% -------------------------------------------------------------------------

-spec connection_start() -> {'ok', pid()} | {'error', any()}.
connection_start() ->
	{ok, Application} = application:get_application(),
	?log_debug("application name: ~p", [Application]),
	{ok, AmqpSpec, _Qos} = parse_opts([]),
	amqp_connection:start(AmqpSpec).


-spec connection_close(pid()) -> 'ok'.
connection_close(Conn) ->
    catch(amqp_connection:close(Conn)),
    ok.

%% -------------------------------------------------------------------------
%% Channel methods
%% -------------------------------------------------------------------------

-spec channel_open(pid()) -> {'ok', pid()} | {'error', any()}.
channel_open(Conn) ->
    amqp_connection:open_channel(Conn).

-spec channel_close(pid()) -> 'ok'.
channel_close(Chan) ->
    catch(amqp_channel:close(Chan)),
    ok.

%% -------------------------------------------------------------------------
%% Queue methods
%% -------------------------------------------------------------------------

-spec queue_declare(pid(), binary(), [{atom(), term()}]) ->
                    'ok' | {'error', any()}.
queue_declare(Chan, Queue, Props) when is_list(Props) ->
    Durable = proplists:get_value(durable, Props, true),
    Exclusive = proplists:get_value(exclusive, Props, false),
    AutoDelete = proplists:get_value(auto_delete, Props, false),
    queue_declare(Chan, Queue, Durable, Exclusive, AutoDelete).

-spec queue_declare(pid(), binary(), boolean(), boolean(), boolean()) ->
                    'ok' | {'error', any()}.
queue_declare(Chan, Queue, Durable, Exclusive, AutoDelete) ->
    Method = #'queue.declare'{queue = Queue,
                              durable = Durable,
                              exclusive = Exclusive,
                              auto_delete = AutoDelete},
    try amqp_channel:call(Chan, Method) of
        #'queue.declare_ok'{} -> ok;
        Other                 -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

%% -------------------------------------------------------------------------
%% Basic methods
%% -------------------------------------------------------------------------

-spec basic_qos(pid(), non_neg_integer()) -> 'ok' | {'error', any()}.
basic_qos(Chan, PrefetchCount) ->
    Method = #'basic.qos'{prefetch_count = PrefetchCount},
    try amqp_channel:call(Chan, Method) of
        #'basic.qos_ok'{} -> ok;
        Other             -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

-spec basic_consume(pid(), binary(), boolean()) ->
                    {'ok', binary()} | {'error', any()}.
basic_consume(Chan, Queue, NoAck) ->
    Method = #'basic.consume'{queue = Queue, no_ack = NoAck},
    try
        amqp_channel:subscribe(Chan, Method, self()),
        receive
            #'basic.consume_ok'{consumer_tag = ConsumerTag} ->
                {ok, ConsumerTag}
        after
            10000 -> {error, timeout}
        end
    catch
        _:Reason -> {error, Reason}
    end.

-spec basic_cancel(pid(), binary()) -> 'ok' | {'error', any()}.
basic_cancel(Chan, ConsumerTag) ->
    Method = #'basic.cancel'{consumer_tag = ConsumerTag},
    try
        amqp_channel:call(Chan, Method),
        receive
            #'basic.cancel_ok'{consumer_tag = ConsumerTag} -> ok
        after
            10000 -> {error, timeout}
        end
    catch
        _:Reason -> {error, Reason}
    end.

-spec basic_publish(pid(), binary(), binary(), #'P_basic'{}) ->
                    'ok' | {'error', any()}.
basic_publish(Chan, RoutingKey, Payload, Props) ->
    Method = #'basic.publish'{routing_key = RoutingKey},
    Content = #amqp_msg{payload = Payload, props = Props},
    try amqp_channel:call(Chan, Method, Content) of
        ok    -> ok;
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

-spec basic_ack(pid(), non_neg_integer()) -> 'ok' | {'error', any()}.
basic_ack(Chan, DeliveryTag) ->
    Method = #'basic.ack'{delivery_tag = DeliveryTag},
    try amqp_channel:call(Chan, Method) of
        ok    -> ok;
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

-spec basic_reject(pid(), non_neg_integer(), boolean()) ->
    'ok' | {'error', any()}.
basic_reject(Chan, DeliveryTag, Requeue) ->
    Method = #'basic.reject'{delivery_tag = DeliveryTag, requeue = Requeue},
    try amqp_channel:call(Chan, Method) of
        ok    -> ok;
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

%% -------------------------------------------------------------------------
%% Tx methods
%% -------------------------------------------------------------------------

-spec tx_select(pid()) -> 'ok' | {'error', any()}.
tx_select(Chan) ->
    Method = #'tx.select'{},
    try amqp_channel:call(Chan, Method) of
        #'tx.select_ok'{} -> ok;
        Other             -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

-spec tx_commit(pid()) -> 'ok' | {'error', any()}.
tx_commit(Chan) ->
    Method = #'tx.commit'{},
    try amqp_channel:call(Chan, Method) of
        #'tx.commit_ok'{} -> ok;
        Other             -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.


parse_opts(undefined) ->
	parse_opts([]);

parse_opts(Opts) ->
	DefaultPropList =
		case application:get_env(k_common, amqp_props) of
			{ok, Value} -> Value;
			undefined -> []
		end,
	?log_debug("DefaultPropList: ~p", [DefaultPropList]),

	%% default amqp props definition
	DHost 		= proplists:get_value(host, DefaultPropList, "127.0.0.1"),
	DPort 		= proplists:get_value(port, DefaultPropList, 5672),
	DVHost 		= proplists:get_value(vhost, DefaultPropList, <<"/">>),
	DUsername 	= proplists:get_value(username, DefaultPropList, <<"guest">>),
	DPass 		= proplists:get_value(password, DefaultPropList, <<"guest">>),
	DQos 		= proplists:get_value(qos, DefaultPropList, 0),

	%% custom amqp props definition
	Host    = proplists:get_value(host, Opts, DHost),
	Port 	= proplists:get_value(port, Opts, DPort),
	VHost 	= proplists:get_value(vhost, Opts, DVHost),
	User 	= proplists:get_value(username, Opts, DUsername),
	Pass 	= proplists:get_value(password, Opts, DPass),
	Qos 	= proplists:get_value(qos, Opts, DQos),

	AmqpSpec = #amqp_params_network{
					username = User,
					password = Pass,
					virtual_host = VHost,
					host = Host,
					port = Port,
					heartbeat = 1
				},
	?log_debug("AmqpSpec: ~p", [AmqpSpec]),
	{ok, AmqpSpec, Qos}.
