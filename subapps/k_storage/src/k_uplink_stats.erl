-module(k_uplink_stats).

-export([get_stats/0]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_stats() -> {ok, Report::term()} | {error, Reason::term()}.
get_stats() ->
	case k_config_api:get_gateways() of
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
prepare_gtws([{GtwUUID, #gateway{}} | Rest], Acc) ->
	{ok, Name} = k_snmp:get_column_val(gtwName, GtwUUID),
	{ok, Status} = k_snmp:get_column_val(gtwStatus, GtwUUID),
	{ok, MaxRPS} = k_snmp:get_column_val(gtwRPS, GtwUUID),
	GtwPropList = [
		{id, GtwUUID},
		{name, Name},
		{status, Status},
		{max_rps, MaxRPS},
		{actual_rps, 'N/A'}
	],
	prepare_gtws(Rest, [GtwPropList | Acc]).
