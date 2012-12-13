-module(k_statistic_detailed_msg_stats_report).

-export([
	get_report/3
]).

-include_lib("k_common/include/gateway.hrl").

-type unix_epoch() :: pos_integer().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(
	FromUnix::unix_epoch(),
	ToUnix::unix_epoch(),
	SliceLength::pos_integer()
) -> {ok, Report::term()} | {error, reason()}.
get_report(FromUnix, ToUnix, SliceLength) when FromUnix < ToUnix ->
	SliceRanges = k_statistic_utils:get_timestamp_ranges(FromUnix, ToUnix, SliceLength),

  	From = k_datetime:unix_epoch_to_timestamp(FromUnix),
	To = k_datetime:unix_epoch_to_timestamp(ToUnix),

	{ok, OutgoingRecords} = get_records(outgoing_messages, From, To),
	OutgoingReport = detailed_msg_stats_report(OutgoingRecords, SliceRanges),

	{ok, IncomingRecords} = get_records(incoming_messages, From, To),
	IncomingReport = detailed_msg_stats_report(IncomingRecords, SliceRanges),

	{ok, {messages, [
		{outgoing, OutgoingReport},
		{incoming, IncomingReport}
	]}}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec get_records(
	Collection::atom(),
	From::erlang:timestamp(),
	To::erlang:timestamp()
) -> {ok, [{gateway_id(), unix_epoch()}]} | {error, reason()}.
get_records(Collection, From, To) ->
	Selector = [ { 'req_time' , { '$gte' , From, '$lt' , To } } ],
	Projector = [ { 'gateway_id' , 1 } , { 'req_time' , 1 } ],
	case mongodb_storage:find(Collection, Selector, Projector) of
		{ok, List} ->
			{ok, [strip_plist(Plist) || {_Id, Plist} <- List]};
		Error ->
			Error
	end.

strip_plist(Plist) ->
	GatewayId = proplists:get_value(gateway_id, Plist),
	ReqTime = proplists:get_value(req_time, Plist),
	ReqTimeUnix = k_datetime:timestamp_to_unix_epoch(ReqTime),
	{GatewayId, ReqTimeUnix}.

-spec detailed_msg_stats_report(
	Records::[{gateway_id(), unix_epoch()}],
	SliceRanges::[{unix_epoch(), unix_epoch()}]
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
										{from, list_to_binary(k_statistic_utils:timestamp_to_iso_8601(F))},
										{to, list_to_binary(k_statistic_utils:timestamp_to_iso_8601(T))},
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

-spec build_gateway_id_to_timestamps_dict([{gateway_id(), unix_epoch()}]) -> dict().
build_gateway_id_to_timestamps_dict(Records) ->
	lists:foldl(
		fun({GatewayId, Timestamp}, Dict) ->
			dict:append(GatewayId, Timestamp, Dict)
		end,
		dict:new(),
		Records).

-spec get_frequencies_from_to(
	Frequencies::[{unix_epoch(), pos_integer()}],
	From::unix_epoch(),
	To::unix_epoch()
) -> [pos_integer()].
get_frequencies_from_to(Frequencies, From, To) ->
	[Fr || {Timestamp, Fr} <- Frequencies, Timestamp >= From, Timestamp < To].
