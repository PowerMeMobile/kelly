-module(k_support_funnel).

-export([
    connections/0,
    disconnect/4,
    throughput/0
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

-type customer_id() :: binary().
-type user_id() :: binary().
-type bind_type() :: transmitter | receiver | transceiver.
-type connection_id() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec connections() -> {ok, [{atom(), term()}]} | {error, term()}.
connections() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, funnel_control_queue),
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #connections_req_v1{
        req_id = ReqId
    },
    ?log_debug("Sending connections request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case k_support_rmq:rpc_call(CtrlQueue, <<"ConnectionsReqV1">>, ReqBin) of
        {ok, <<"ConnectionsRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#connections_resp_v1{}, RespBin),
            ?log_debug("Got connections response: ~p", [Resp]),
            Conns = Resp#connections_resp_v1.connections,
            {ok, prepare_conns(Conns)};
        {error, Error} ->
            {error, Error}
    end.

-spec disconnect(customer_id(), user_id(), bind_type(), connection_id()) ->
    ok | {error, term()}.
disconnect(CustomerId, UserId, BindType, ConnectionId) ->
    {ok, CtrlQueue} = application:get_env(k_handlers, funnel_control_queue),
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #disconnect_req_v1{
        req_id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        bind_type = BindType,
        connection_id = ConnectionId
    },
    ?log_debug("Sending disconnect request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case k_support_rmq:rpc_call(CtrlQueue, <<"DisconnectReqV1">>, ReqBin) of
        {ok, <<"DisconnectRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#disconnect_resp_v1{}, RespBin),
            ?log_debug("Got disconnect response: ~p", [Resp]),
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec throughput() -> {ok, [{atom(), term()}]} | {error, term()}.
throughput() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, funnel_control_queue),
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #throughput_req_v1{
        req_id = ReqId
    },
    ?log_debug("Sending throughput request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case k_support_rmq:rpc_call(CtrlQueue, <<"ThroughputReqV1">>, ReqBin) of
        {ok, <<"ThroughputRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#throughput_resp_v1{}, RespBin),
            ?log_debug("Got throughput response: ~p", [Resp]),
            Slices = Resp#throughput_resp_v1.slices,
            {ok, prepare_slices(Slices)};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

prepare_conns(Conns) ->
    prepare_conns(Conns, []).

prepare_conns([], Acc) ->
    lists:reverse(Acc);
prepare_conns([#connection_v1{
    connection_id = ConnectionId,
    remote_ip = RemoteIP,
    customer_id = CustomerId,
    user_id = UserId,
    connected_at = ConnectedAt,
    bind_type = BindType,
    msgs_received = MsgsReceived,
    msgs_sent = MsgsSent,
    errors = Errors
} | Conns], Acc) ->
    RemoteIP2 = list_to_binary(inet:ntoa(RemoteIP)),
    ConnectedAtISO = ac_datetime:timestamp_to_iso8601(ConnectedAt),
    Plist = [
        {connection_id, ConnectionId},
        {remote_ip, RemoteIP2},
        {customer_id, CustomerId},
        {user_id, UserId},
        {connected_at, ConnectedAtISO},
        {bind_type, BindType},
        {msgs_received, MsgsReceived},
        {msgs_sent, MsgsSent},
        {errors, [error_to_plist(Error) || Error <- Errors]}
    ],
    prepare_conns(Conns, [Plist | Acc]).

error_to_plist(#connection_error_v1{error_code = Code, timestamp = TS}) ->
    [
        {error_code, Code},
        {timestamp, ac_datetime:timestamp_to_iso8601(TS)}
    ].

prepare_slices(Slices) ->
    prepare_slices(Slices, []).

prepare_slices([], Acc) ->
    lists:reverse(Acc);
prepare_slices([Slice | Slices], Acc) ->
    PeriodStart = Slice#throughput_slice_v1.period_start,
    Counters = Slice#throughput_slice_v1.counters,
    Plist = [
        {period_start, ac_datetime:timestamp_to_iso8601(PeriodStart)},
        {counters, [counter_to_plist(C) || C <- Counters]}
    ],
    prepare_slices(Slices, [Plist | Acc]).

counter_to_plist(#throughput_counter_v1{
    connection_id = ConnectionId,
    direction = Direction,
    count = Count
}) ->
    [
        {connection_id, ConnectionId},
        {direction, Direction},
        {count, Count}
    ].
