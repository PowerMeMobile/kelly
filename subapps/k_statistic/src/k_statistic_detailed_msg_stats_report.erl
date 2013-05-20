-module(k_statistic_detailed_msg_stats_report).

-export([
	get_report/3
]).

-include_lib("k_common/include/gateway.hrl").

-type unixepoch() :: pos_integer().
-type report() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(unixepoch(), unixepoch(), pos_integer()) ->
	{ok, report()} | {error, reason()}.
get_report(FromUnix, ToUnix, SliceLength) when FromUnix < ToUnix ->
	SliceRanges = k_statistic_utils:get_timestamp_ranges(FromUnix, ToUnix, SliceLength),

  	From = k_datetime:unixepoch_to_timestamp(FromUnix),
	To = k_datetime:unixepoch_to_timestamp(ToUnix),

	{ok, MtRecords} = get_records(mt_messages, From, To),
	MtReport = detailed_msg_stats_report(MtRecords, SliceRanges),

	{ok, MoRecords} = get_records(mo_messages, From, To),
	MoReport = detailed_msg_stats_report(MoRecords, SliceRanges),

	{ok, [{outgoing, MtReport},{incoming, MoReport}]}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec get_records(
	Collection::atom(),
	From::erlang:timestamp(),
	To::erlang:timestamp()
) -> {ok, [{gateway_id(), unixepoch()}]} | {error, reason()}.
get_records(Collection, From, To) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		}
	},
	Projector = {
		'gi'  , 1,
		'rqt' , 1
	},
	case k_shifted_storage:find(Collection, Selector, Projector) of
		{ok, Docs} ->
			{ok, [strip_doc(Doc) || {_Id, Doc} <- Docs]};
		Error ->
			Error
	end.

strip_doc(Doc) ->
	GatewayId = bsondoc:at(gi, Doc),
	ReqTime = bsondoc:at(rqt, Doc),
	ReqTimeUnix = k_datetime:timestamp_to_unixepoch(ReqTime),
	{GatewayId, ReqTimeUnix}.

-spec detailed_msg_stats_report(
	Records::[{gateway_id(), unixepoch()}],
	SliceRanges::[{unixepoch(), unixepoch()}]
) -> [tuple()].
detailed_msg_stats_report(Records, SliceRanges) ->
	Total = length(Records),
	Dict = build_gateway_id_to_timestamps_dict(Records),
	GatewayIds = dict:fetch_keys(Dict),
	[
		{total, Total},
		{gateways,
			lists:map(
				fun(GatewayId) ->
					Timestamps = dict:fetch(GatewayId, Dict),
					Frequencies = k_lists:make_frequencies(Timestamps),
					GatewayTotal = length(Timestamps),
					[
						{gateway_id, GatewayId},
						{gateway_name, get_gateway_name(GatewayId)},
						{total, GatewayTotal},
						{slices,
							lists:map(
								fun({F, T}) ->
									SliceFreqs = get_frequencies_from_to(Frequencies, F, T),
									SliceTotal = lists:sum(SliceFreqs),
									SliceAvg = SliceTotal / (T - F),
									SlicePeak = if
										SliceTotal =:= 0 -> 0.0;
										true -> lists:max(SliceFreqs) * 1.0
									end,
									[
										{from, k_datetime:unixepoch_to_iso8601(F)},
										{to, k_datetime:unixepoch_to_iso8601(T)},
										{total, SliceTotal},
										{avg, SliceAvg},
										{peak, SlicePeak}
									]
								end,
								SliceRanges)
						}
					]
				end,
			GatewayIds)
		}
	].

-spec get_gateway_name(GatewayId::gateway_id()) -> string().
get_gateway_name(GatewayId) ->
	case k_config:get_gateway(GatewayId) of
		{ok, #gateway{name = Name}} ->
			Name;
		_ ->
			"N/A"
	end.

-spec build_gateway_id_to_timestamps_dict([{gateway_id(), unixepoch()}]) -> dict().
build_gateway_id_to_timestamps_dict(Records) ->
	lists:foldl(
		fun({GatewayId, Timestamp}, Dict) ->
			dict:append(GatewayId, Timestamp, Dict)
		end,
		dict:new(),
		Records).

-spec get_frequencies_from_to(
	Frequencies::[{unixepoch(), pos_integer()}],
	From::unixepoch(),
	To::unixepoch()
) -> [pos_integer()].
get_frequencies_from_to(Frequencies, From, To) ->
	[Fr || {Timestamp, Fr} <- Frequencies, Timestamp >= From, Timestamp < To].
