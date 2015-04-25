-module(k_support_just).

-export([
    reconfigure/0,
    throughput/0,

    block_request/1,
    unblock_request/1,

    set_customer/3,
    delete_customer/1,

    set_gateway/3,
    delete_gateway/1,

    set_connection/2,
    delete_connection/2,

    set_setting/2,
    delete_setting/2
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
    [set_customer(
        C#customer.customer_uuid, C#customer.rps, C#customer.priority
    ) || C <- Customers],
    {ok, Gtws} = k_storage_gateways:get_gateways(),
    [set_gtw(Gtw) || Gtw <- Gtws],
    ok.

-spec throughput() -> {ok, [{atom(), term()}]} | {error, term()}.
throughput() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, just_control_queue),
    {ok, ReqBin} =
        'JustAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
    ?log_debug("Sending just connections request", []),
    case k_support_rmq:rpc_call(CtrlQueue, <<"ThroughputRequest">>, ReqBin) of
        {ok, <<"ThroughputResponse">>, RespBin} ->
            {ok, Resp = #'ThroughputResponse'{slices = Slices}} =
                'JustAsn':decode('ThroughputResponse', RespBin),
            ?log_debug("Got just throughput response: ~p", [Resp]),
            {ok, prepare_slices(Slices)};
        {error, Error} ->
            {error, Error}
    end.

-spec block_request(binary()) -> ok | {error, term()}.
block_request(ReqId) ->
    {ok, CtrlQueue} = application:get_env(k_handlers, just_control_queue),
    Req = #block_req_v1{
        req_id = uuid:unparse(uuid:generate()),
        sms_req_id = ReqId
    },
    {ok, ReqBin} = adto:encode(Req),
    case k_support_rmq:rpc_call(CtrlQueue, <<"BlockReqV1">>, ReqBin) of
        {ok, <<"BlockRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#block_resp_v1{}, RespBin),
            #block_resp_v1{result = Result} = Resp,
            Result;
        {error, Error} ->
            {error, Error}
    end.

-spec unblock_request(binary()) -> ok | {error, term()}.
unblock_request(ReqId) ->
    {ok, CtrlQueue} = application:get_env(k_handlers, just_control_queue),
    Req = #unblock_req_v1{
        req_id = uuid:unparse(uuid:generate()),
        sms_req_id = ReqId
    },
    {ok, ReqBin} = adto:encode(Req),
    case k_support_rmq:rpc_call(CtrlQueue, <<"UnblockReqV1">>, ReqBin) of
        {ok, <<"UnblockRespV1">>, RespBin} ->
            {ok, Resp} = adto:decode(#unblock_resp_v1{}, RespBin),
            #unblock_resp_v1{result = Result} = Resp,
            Result;
        {error, Error} ->
            {error, Error}
    end.

-spec set_customer(binary(), integer(), integer()) -> ok.
set_customer(ID, RPS, Priority) when
        is_binary(ID) andalso is_integer(RPS) andalso is_integer(Priority) ->
    k_support_just_snmp:set_row(cst, binary_to_list(ID), [
        {cstRPS, RPS},
        {cstPriority, Priority}
    ]).

-spec delete_customer(binary()) -> ok.
delete_customer(ID) when is_binary(ID) ->
    k_support_just_snmp:del_row(cst, binary_to_list(ID)).

-spec set_gateway(binary(), binary(), integer()) -> ok.
set_gateway(ID, Name, RPS) when
        is_binary(ID) andalso is_binary(Name) andalso is_integer(RPS) ->
    k_support_just_snmp:set_row(gtw, binary_to_list(ID), [
        {gtwName, binary_to_list(Name)}, {gtwRPS, RPS}
    ]).

-spec delete_gateway(binary()) -> ok.
delete_gateway(ID) when is_binary(ID) ->
    k_support_just_snmp:del_row(gtw, binary_to_list(ID)).

-spec set_connection(binary(), #connection{}) -> ok.
set_connection(GtwID, Conn = #connection{}) when is_binary(GtwID) ->
    k_support_just_snmp:set_row(cnn, binary_to_list(GtwID) ++ [Conn#connection.id], [
        {cnnAddr, binary_to_list(Conn#connection.host)},
        {cnnPort, Conn#connection.port},
        {cnnType, bind_type_to_integer(Conn#connection.bind_type)},
        {cnnSystemId, binary_to_list(Conn#connection.system_id)},
        {cnnPassword, binary_to_list(Conn#connection.password)},
        {cnnSystemType, binary_to_list(Conn#connection.system_type)},
        {cnnAddrTon, Conn#connection.addr_ton},
        {cnnAddrNpi, Conn#connection.addr_npi},
        {cnnAddrRange, binary_to_list(Conn#connection.addr_range)}
    ]).

-spec delete_connection(binary(), integer()) -> ok.
delete_connection(GtwID, ConnID) when
        is_binary(GtwID) andalso is_integer(ConnID) ->
    k_support_just_snmp:del_row(cnn, binary_to_list(GtwID) ++ [ConnID]).

-spec set_setting(binary(), #setting{}) -> ok.
set_setting(GtwID, Setting = #setting{}) when is_binary(GtwID) ->
    Index = binary_to_list(GtwID) ++
            [size(Setting#setting.name)] ++
            binary_to_list(Setting#setting.name),
    k_support_just_snmp:set_row(sts, Index, [
        {stsValue, binary_to_list(Setting#setting.value)}
    ]).

-spec delete_setting(binary(), integer()) -> ok.
delete_setting(GtwID, SettingID) when
        is_binary(GtwID) andalso is_binary(SettingID) ->
    Index = binary_to_list(GtwID) ++ [size(SettingID)] ++ binary_to_list(SettingID),
    k_support_just_snmp:del_row(sts, Index).

%% ===================================================================
%% Internal
%% ===================================================================

set_gtw(Gtw) ->
    GtwId = Gtw#gateway.id,
    ok = set_gateway(GtwId, Gtw#gateway.name, Gtw#gateway.rps),
    [ok = set_connection(GtwId, Conn) || Conn <- Gtw#gateway.connections],
    [ok = set_setting(GtwId, Setting) || Setting <- Gtw#gateway.settings].

prepare_slices(Slices) ->
    prepare_slices(Slices, []).

prepare_slices([], Acc) ->
    lists:reverse(Acc);
prepare_slices([Slice | Slices], Acc) ->
    PeriodStart = Slice#'Slice'.periodStart,
    Counters = Slice#'Slice'.counters,
    Plist = [
        {period_start, ac_datetime:utc_string_to_iso8601(PeriodStart)},
        {counters, [counter_to_plist(C) || C <- Counters]}
    ],
    prepare_slices(Slices, [Plist | Acc]).

counter_to_plist(#'Counter'{
    gatewayId = GatewayId,
    gatewayName = GatewayName,
    type = Type,
    count = Count
}) ->
    [
        {gateway_id, list_to_binary(GatewayId)},
        {gateway_name, list_to_binary(GatewayName)},
        {direction, direction(Type)},
        {count, Count}
    ].

direction('smsOut') -> out;
direction('smsIn')  -> in.

bind_type_to_integer(transmitter) -> 1;
bind_type_to_integer(receiver)    -> 2;
bind_type_to_integer(transceiver) -> 3.
