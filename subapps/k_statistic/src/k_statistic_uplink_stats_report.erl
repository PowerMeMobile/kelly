-module(k_statistic_uplink_stats_report).

-export([
	get_report/0
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report() -> {ok, Report::term()} | {error, Reason::term()}.
get_report() ->
	case k_config:get_gateways() of
		{ok, GtwList} ->
			{ok, GtwPropLists} = prepare_gtws(GtwList),
			%?log_debug("GtwPropLists: ~p", [GtwPropLists]),
			Report = {gateways, GtwPropLists},
			{ok, Report};
		{error, Error} ->
			{error, Error}
	end.

%% ===================================================================
%% Internal
%% ===================================================================

prepare_gtws(GtwList) when is_list(GtwList) ->
	prepare_gtws(GtwList, []);
prepare_gtws(Gtw = {_UUID, #gateway{}}) ->
	prepare_gtws([Gtw], []).

prepare_gtws([], Acc) ->
	{ok, Acc};
prepare_gtws([{GtwUUIDBin, #gateway{}} | Rest], Acc) ->
	GtwUUID = k_uuid:to_string(GtwUUIDBin),
	{ok, Name} = k_snmp:get_column_val(gtwName, GtwUUID),
	{ok, Status} = k_snmp:get_column_val(gtwStatus, GtwUUID),
	{ok, MaxRPS} = k_snmp:get_column_val(gtwRPS, GtwUUID),
	GtwPropList = [
		{id, list_to_binary(GtwUUID)},
		{name, list_to_binary(Name)},
		{status, Status},
		{max_rps, MaxRPS},
		{actual_rps, 'N/A'}
	],
	prepare_gtws(Rest, [GtwPropList | Acc]).