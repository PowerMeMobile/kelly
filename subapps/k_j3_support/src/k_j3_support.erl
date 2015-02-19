-module(k_j3_support).

-export([
    reconfigure/0,
    get_throughput/0,
    block_request/1,
    unblock_request/1,

    %% TODO: find a better place for it.
    get_funnel_connections/0
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_dto/include/common_dto.hrl").
-include_lib("alley_dto/include/JustAsn.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/gateway.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec reconfigure() -> ok.
reconfigure() ->
    {ok, Customers} = k_storage_customers:get_customers(),
    [k_snmp:set_customer(
        C#customer.customer_uuid, C#customer.rps, C#customer.priority
    ) || C <- Customers],
    {ok, Gtws} = k_storage_gateways:get_gateways(),
    [set_gtw(Gtw) || Gtw <- Gtws],
    ok.

-spec get_throughput() -> {ok, [{atom(), term()}]} | {error, term()}.
get_throughput() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, just_control_queue),
    {ok, ReqBin} =
        'JustAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
    case k_j3_support_rmq:rpc_call(CtrlQueue, <<"ThroughputRequest">>, ReqBin) of
        {ok, <<"ThroughputResponse">>, RespBin} ->
            {ok, #'ThroughputResponse'{slices = Slices}} =
                'JustAsn':decode('ThroughputResponse', RespBin),
            Counters =
                lists:flatten(
                    lists:reverse([Slice#'Slice'.counters || Slice <- Slices])),
            case k_storage_gateways:get_gateways() of
                {ok, GtwList} ->
                    {ok, _GtwPropLists} = prepare_gtws(Counters, GtwList);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec block_request(binary()) -> ok | {error, term()}.
block_request(ReqId) ->
    Req = #block_req_v1{
        req_id = uuid:unparse(uuid:generate()),
        sms_req_id = ReqId
    },
    {ok, ReqBin} = adto:encode(Req),
    case k_j3_support_rmq:rpc_call(<<"BlockReqV1">>, ReqBin) of
        {ok, <<"BlockRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#block_resp_v1{}, RespBin),
            #block_resp_v1{result = Result} = Resp,
            Result;
        {error, Error} ->
            {error, Error}
    end.

-spec unblock_request(binary()) -> ok | {error, term()}.
unblock_request(ReqId) ->
    Req = #unblock_req_v1{
        req_id = uuid:unparse(uuid:generate()),
        sms_req_id = ReqId
    },
    {ok, ReqBin} = adto:encode(Req),
    case k_j3_support_rmq:rpc_call(<<"UnblockReqV1">>, ReqBin) of
        {ok, <<"UnblockRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#unblock_resp_v1{}, RespBin),
            #unblock_resp_v1{result = Result} = Resp,
            Result;
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% TODO: Funnel specific
%% ===================================================================

-spec get_funnel_connections() -> {ok, [{atom(), term()}]} | {error, term()}.
get_funnel_connections() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, funnel_control_queue),
    {ok, ReqBin} = adto:encode(#funnel_connections_request_dto{}),
    case k_j3_support_rmq:rpc_call(CtrlQueue, <<"ConnectionsRequest">>, ReqBin) of
        {ok, <<"ConnectionsResponse">>, RespBin} ->
            {ok, #funnel_connections_response_dto{connections = Connections}} =
                adto:decode(#funnel_connections_response_dto{}, RespBin),
            {ok, _ConnectionPropLists} = prepare_conns(Connections);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

set_gtw(Gtw) ->
    GtwId = Gtw#gateway.id,
    k_snmp:set_gateway(GtwId, Gtw#gateway.name, Gtw#gateway.rps),
    [k_snmp:set_connection(GtwId, Conn) || Conn <- Gtw#gateway.connections],
    [k_snmp:set_setting(GtwId, Setting) || Setting <- Gtw#gateway.settings].

prepare_gtws(Counters, GtwList) when is_list(GtwList) ->
    prepare_gtws(Counters, GtwList, []);
prepare_gtws(Counters, Gtw = #gateway{}) ->
    prepare_gtws(Counters, [Gtw], []).

prepare_gtws(_Counters, [], Acc) ->
    {ok, Acc};
prepare_gtws(Counters, [#gateway{id = GtwUuidBin} | Rest], Acc) ->
    GtwUuid = binary_to_list(GtwUuidBin),
    {ok, Name} = k_snmp:get_column_val(gtwName, GtwUuid),
    {ok, Status} = k_snmp:get_column_val(gtwStatus, GtwUuid),
    {ok, MaxRPS} = k_snmp:get_column_val(gtwRPS, GtwUuid),
    {ok, ActualRpsIn} = get_actual_rps_sms(smsIn, GtwUuid, Counters),
    {ok, ActualRpsOut} = get_actual_rps_sms(smsOut, GtwUuid, Counters),
    GtwPropList = [
        {id, GtwUuidBin},
        {name, list_to_binary(Name)},
        {status, Status},
        {max_rps, MaxRPS},
        {actual_rps_in, ActualRpsIn},
        {actual_rps_out, ActualRpsOut}
    ],
    prepare_gtws(Counters, Rest, [GtwPropList | Acc]).

get_actual_rps_sms(_Type, _Uuid, []) ->
    {ok, 0};
get_actual_rps_sms(Type, Uuid, [#'Counter'{gatewayId = Uuid, type = Type, count = Count} | _]) ->
    {ok, Count};
get_actual_rps_sms(Type, Uuid, [_| Rest]) ->
    get_actual_rps_sms(Type, Uuid, Rest).

%% ===================================================================
%% TODO: Internal Funnel specific
%% ===================================================================

prepare_conns(ConnList) when is_list(ConnList) ->
    prepare_conns(ConnList, []).

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
