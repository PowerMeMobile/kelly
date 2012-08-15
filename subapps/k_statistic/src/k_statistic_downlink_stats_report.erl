-module(k_statistic_downlink_stats_report).

-export([
	get_report/0
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("k_common/include/FunnelAsn.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report() -> {ok, Report::term()} | {error, Reason::term()}.
get_report() ->
	%% connect and make channel.
	Params = #amqp_params_network{
		host = "127.0.0.1",
		port = 5672,
		username = <<"guest">>,
		password = <<"guest">>,
		virtual_host = <<"/">>
	},
	{ok, Conn} = amqp_connection:start(Params),
	{ok, Channel} = amqp_connection:open_channel(Conn),

	%% declare `To' queue.
	QueueTo = <<"pmm.funnel.server_control">>,
	DeclareTo = #'queue.declare'{
		queue = QueueTo,
		durable = false,
		exclusive = false,
		auto_delete = true},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, DeclareTo),
	PublishMethod = #'basic.publish'{routing_key = QueueTo},

	%% declate `ReplyTo' queue.
	#'queue.declare_ok'{queue = QueueReplyTo} = amqp_channel:call(Channel, #'queue.declare'{}),

	%% send `Connections' request.
	ConnectionsProps = #'P_basic'{
		content_type = <<"ConnectionsRequest">>,
		message_id = k_uuid:newid(),
		reply_to = QueueReplyTo
	},
	{ok, ConnectionsRequest} = 'FunnelAsn':encode('ConnectionsRequest', #'ConnectionsRequest'{}),
	ConnectionsContent = #amqp_msg{payload = list_to_binary(ConnectionsRequest), props = ConnectionsProps},
	amqp_channel:cast(Channel, PublishMethod, ConnectionsContent),

	%% get `Connections' response.
	{ok, ConnectionsResponse} = get_response(Channel, QueueReplyTo),
	{ok, #'ConnectionsResponse'{connections = Connections}} =
		'FunnelAsn':decode('ConnectionsResponse', ConnectionsResponse),
	io:format("~p~n", [Connections]),

	%% %% send `Throughput' request.
	%% ThroughputProps = #'P_basic'{
	%% 	content_type = <<"ThroughputRequest">>,
	%% 	message_id = k_uuid:newid(),
	%% 	reply_to = QueueReplyTo
	%% },
	%% {ok, ThroughputRequest} = 'FunnelAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
	%% ThroughputContent = #amqp_msg{payload = list_to_binary(ThroughputRequest), props = ThroughputProps},
	%% amqp_channel:cast(Channel, PublishMethod, ThroughputContent),

	%% %% get `Throughput' response.
	%% {ok, ThroughputResponse} = get_response(Channel, QueueReplyTo),
	%% {ok, Throughput} = 'FunnelAsn':decode('ThroughputResponse', ThroughputResponse),
	%% io:format("~p~n", [Throughput]),

	%% delete `ReplyTo' queue.
    Delete = #'queue.delete'{queue = QueueReplyTo},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete),

	%% close channel and connection.
	ok = amqp_channel:close(Channel),
	ok = amqp_connection:close(Conn),

	{ok, ConnectionPropLists} = prepare_conns(Connections),
	Report = {connections, ConnectionPropLists},
	{ok, Report}.

%% ===================================================================
%% Internal
%% ===================================================================

get_response(Channel, QueueReplyTo) ->
	case amqp_channel:call(Channel, #'basic.get'{queue = QueueReplyTo}) of
		#'basic.get_empty'{} ->
			timer:sleep(100),
			get_response(Channel, QueueReplyTo);
		{#'basic.get_ok'{}, #amqp_msg{payload = RespPayload}} ->
			{ok, binary_to_list(RespPayload)}
	end.

prepare_conns(ConnList) when is_list(ConnList) ->
	prepare_conns(ConnList, []).
%% prepare_conns(Gtw = {_UUID, #gateway{}}) ->
%% 	prepare_gtws([Gtw], []).

prepare_conns([], Acc) ->
	{ok, Acc};
prepare_conns([#'Connection'{
	connectionId = ConnectionId,
	remoteIp = RemoteIp,
	customerId = CustomerId,
	userId = UserId,
	connectedAt = ConnectedAt,
	type = Type,
	msgsReceived = MsgsReceived,
	msgsSent = MsgsSent,
	errors = Errors
} | Rest], Acc) ->
	ConnPropList = [
		{id, list_to_binary(ConnectionId)},
		{remote_ip, list_to_binary(RemoteIp)},
		{customer_id, list_to_binary(CustomerId)},
		{user_id, list_to_binary(UserId)},
		{connected_at, list_to_binary(ConnectedAt)},
		{type, Type},
		{msgs_received, MsgsReceived},
		{msgs_sent, MsgsSent},
		{errors, list_to_binary(Errors)}
	],
	prepare_conns(Rest, [ConnPropList | Acc]).
