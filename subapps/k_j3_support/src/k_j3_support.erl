-module(k_j3_support).

-export([
    reconfigure/0,
    get_throughput/0
]).

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

-spec get_throughput() -> ok.
get_throughput() ->
    {ok, ReqBin} =
        'JustAsn':encode('ThroughputRequest', #'ThroughputRequest'{}),
    case k_j3_support_rmq:rpc_call(<<"ThroughputRequest">>, ReqBin) of
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
