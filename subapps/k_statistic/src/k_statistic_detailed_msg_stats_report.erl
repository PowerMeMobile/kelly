-module(k_statistic_detailed_msg_stats_report).

-export([
	get_report/3
]).

-include("msg_stats.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(
	From::calendar:datetime(),
	To::calendar:datetime(),
	SliceLength::pos_integer()
) -> {ok, Report::term()} | {error, Reason::term()}.
get_report(From, To, SliceLength) when From < To ->
	SliceRanges = k_statistic_utils:get_timestamp_ranges(From, To, SliceLength),

	OutgoingFilenames = k_statistic_utils:get_file_list_with(
		From, To, fun k_statistic_utils:msg_stats_slice_path/1),
	OutgoingRecords = get_msg_stats_records(OutgoingFilenames),
	OutgoingReport = detailed_msg_stats_report(OutgoingRecords, SliceRanges),

	IncomingFilenames = k_statistic_utils:get_file_list_with(
		From, To, fun k_statistic_utils:incoming_msg_stats_slice_path/1),
	IncomingRecords = get_msg_stats_records(IncomingFilenames),
	IncomingReport = detailed_msg_stats_report(IncomingRecords, SliceRanges),

	{ok, {messages, [
		{outgoing, OutgoingReport},
		{incoming, IncomingReport}
	]}}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec get_msg_stats_records([file:filename()]) -> [{gateway_id(), os:timestamp()}].
get_msg_stats_records(Filenames) ->
	lists:foldr(
		fun(Filename, SoFar) ->
			case k_statistic_utils:read_term_from_file(Filename) of
				{ok, []} ->
					SoFar;
				{ok, Records} ->
					StripedRecords = strip_msg_stats(Records),
					StripedRecords ++ SoFar;
				{error, _Reason} ->
					%?log_debug("Missing msg stats report: ~p", [Filename])
					SoFar
			end
		end,
		[],
		Filenames).

-spec strip_msg_stats([#msg_stats{}]) -> [{gateway_id(), os:timestamp()}].
strip_msg_stats(Records) ->
	lists:map(
		fun(#msg_stats{
			msg_info = #msg_info{gateway_id = GatewayId},
			time = Time
		}) ->
			{GatewayId, Time}
		end,
		Records).

-spec detailed_msg_stats_report(
	Records::[{gateway_id(), os:timestamp()}],
	SliceRanges::[{os:timestamp(), os:timestamp()}]
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
					Frequencies = k_statistic_utils:make_frequencies(Timestamps),
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
										{from, k_statistic_utils:timestamp_to_iso_8601(F)},
										{to, k_statistic_utils:timestamp_to_iso_8601(T)},
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

-spec build_gateway_id_to_timestamps_dict([{gateway_id(), os:timestamp()}]) -> dict().
build_gateway_id_to_timestamps_dict(Records) ->
	lists:foldl(
		fun({GatewayId, Timestamp}, Dict) ->
			dict:append(GatewayId, Timestamp, Dict)
		end,
		dict:new(),
		Records).

-spec get_frequencies_from_to(
	Frequencies::[{os:timestamp(), pos_integer()}],
	From::os:timestamp(),
	To::os:timestamp()
) -> [pos_integer()].
get_frequencies_from_to(Frequencies, From, To) ->
	MoreThenFrom = lists:dropwhile(fun({Timestamp, _}) -> Timestamp < From end, Frequencies),
	LessThenTo = lists:takewhile(fun({Timestamp, _}) -> Timestamp < To end, MoreThenFrom),
	lists:map(fun({_, Fr}) -> Fr end, LessThenTo).
