-module(k_statistic_downlink_stats_report).

-export([
    get_report/0
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/adto.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report() -> {ok, Report::term()} | {error, Reason::term()}.
get_report() ->
    %% connect and make channel.
    {ok, Channel} = rmql:channel_open(),

    %% declare `To' queue.
    {ok, QueueTo} = application:get_env(k_handlers, funnel_control_queue),
    %% DeclareTo = #'queue.declare'{
    %%  queue = QueueTo,
    %%  durable = false,
    %%  exclusive = false,
    %%  auto_delete = true},
    %% #'queue.declare_ok'{} = amqp_channel:call(Channel, DeclareTo),
    PublishMethod = #'basic.publish'{routing_key = QueueTo},

    %% declate `ReplyTo' queue.
    #'queue.declare_ok'{queue = QueueReplyTo} = amqp_channel:call(Channel, #'queue.declare'{}),

    %% send `Connections' request.
    ConnectionsProps = #'P_basic'{
        content_type = <<"ConnectionsRequest">>,
        message_id = uuid:unparse(uuid:generate_time()),
        reply_to = QueueReplyTo
    },
    {ok, ConnectionsRequest} = adto:encode(#funnel_connections_request_dto{}),
    ConnectionsContent = #amqp_msg{payload = ConnectionsRequest, props = ConnectionsProps},
    amqp_channel:cast(Channel, PublishMethod, ConnectionsContent),

    %% get `Connections' response.
    {ok, ConnectionsResponse} = get_response(Channel, QueueReplyTo),
    {ok, #funnel_connections_response_dto{connections = Connections}} =
        adto:decode(#funnel_connections_response_dto{}, ConnectionsResponse),

    %% %% send `Throughput' request.
    %% ThroughputProps = #'P_basic'{
    %%  content_type = <<"ThroughputRequest">>,
    %%  message_id = uuid:unparse(uuid:generate_time()),
    %%  reply_to = QueueReplyTo
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
    rmql:channel_close(Channel),

    {ok, _ConnectionPropLists} = prepare_conns(Connections).

%% ===================================================================
%% Internal
%% ===================================================================

get_response(Channel, QueueReplyTo) ->
    case amqp_channel:call(Channel, #'basic.get'{queue = QueueReplyTo}) of
        #'basic.get_empty'{} ->
            timer:sleep(100),
            get_response(Channel, QueueReplyTo);
        {#'basic.get_ok'{}, #amqp_msg{payload = RespPayload}} ->
            {ok, RespPayload}
    end.

prepare_conns(ConnList) when is_list(ConnList) ->
    prepare_conns(ConnList, []).
%% prepare_conns(Gtw = {_Uuid, #gateway{}}) ->
%%  prepare_gtws([Gtw], []).

prepare_conns([], Acc) ->
    {ok, Acc};
prepare_conns([#funnel_connection_dto{
    connection_id = ConnectionId,
    remote_ip = RemoteIp,
    customer_id = CustomerId,
    user_id = UserId,
    connected_at = ConnectedAt,
    type = Type,
    msgs_received = MsgsReceived,
    msgs_sent = MsgsSent,
    errors = Errors
} | Rest], Acc) ->
    ConnectedAtDT = ac_datetime:utc_string_to_datetime(ConnectedAt),
    ConnectedAtISO = ac_datetime:datetime_to_iso8601(ConnectedAtDT),
    ConnPropList = [
        {id, ConnectionId},
        {remote_ip, RemoteIp},
        {customer_id, CustomerId},
        {user_id, UserId},
        {connected_at, ConnectedAtISO},
        {type, Type},
        {msgs_received, MsgsReceived},
        {msgs_sent, MsgsSent},
        {errors, [error_to_proplist(Error) || Error <- Errors]}
    ],
    prepare_conns(Rest, [ConnPropList | Acc]).

error_to_proplist(#error_dto{error_code = ErrorCode, timestamp = Timestamp}) ->
    [
        {error_code, ErrorCode},
        {timestamp, Timestamp}
    ].
