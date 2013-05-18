-module(k_statistic_uplink_stats_report).

-export([
	get_report/0
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gateway.hrl").
-include_lib("alley_dto/include/JustAsn.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report() -> {ok, Report::term()} | {error, Reason::term()}.
get_report() ->
	case k_config:get_gateways() of
		{ok, GtwList} ->
			{ok, #'ThroughputResponse'{slices = Slices}} =
				k_statistic_uplink_stats_report_helper:get_gtws_throughput(),
			Counters =
				lists:flatten(lists:reverse([Slice#'Slice'.counters || Slice <- Slices])),
			{ok, GtwPropLists} = prepare_gtws(Counters, GtwList),
			Report = {gateways, GtwPropLists},
			{ok, Report};
		{error, Error} ->
			{error, Error}
	end.

%% ===================================================================
%% Internal
%% ===================================================================

prepare_gtws(Counters, GtwList) when is_list(GtwList) ->
	prepare_gtws(Counters, GtwList, []);
prepare_gtws(Counters, Gtw = {_UUID, #gateway{}}) ->
	prepare_gtws(Counters, [Gtw], []).

prepare_gtws(_Counters, [], Acc) ->
	{ok, Acc};
prepare_gtws(Counters, [{GtwUUIDBin, #gateway{}} | Rest], Acc) ->
	GtwUUID = binary_to_list(GtwUUIDBin),
	{ok, Name} = k_snmp:get_column_val(gtwName, GtwUUID),
	{ok, Status} = k_snmp:get_column_val(gtwStatus, GtwUUID),
	{ok, MaxRPS} = k_snmp:get_column_val(gtwRPS, GtwUUID),
	{ok, ActualRpsIn} = get_actual_rps_sms(smsIn, GtwUUID, Counters),
	{ok, ActualRpsOut} = get_actual_rps_sms(smsOut, GtwUUID, Counters),
	GtwPropList = [
		{id, GtwUUIDBin},
		{name, list_to_binary(Name)},
		{status, Status},
		{max_rps, MaxRPS},
		{actual_rps_in, ActualRpsIn},
		{actual_rps_out, ActualRpsOut}
	],
	prepare_gtws(Counters, Rest, [GtwPropList | Acc]).

get_actual_rps_sms(_Type, _UUID, []) ->
	{ok, 0};
get_actual_rps_sms(Type, UUID, [#'Counter'{gatewayId = UUID, type = Type, count = Count} | _]) ->
	{ok, Count};
get_actual_rps_sms(Type, UUID, [_| Rest]) ->
	get_actual_rps_sms(Type, UUID, Rest).
