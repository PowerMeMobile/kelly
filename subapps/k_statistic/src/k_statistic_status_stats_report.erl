-module(k_statistic_status_stats_report).

-export([
	status_stats_report/3
]).

-include("status_stats.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec status_stats_report(From::os:timestamp(), To::os:timestamp(), Status::atom()) -> [tuple()].
status_stats_report(From, To, Status) ->
	Filenames = k_statistic_utils:get_file_list(
		From, To, fun k_statistic_utils:status_stats_slice_path/1),
	AllRecords =
		lists:foldr(
			fun(Filename, SoFar) ->
				case k_statistic_utils:read_term_from_file(Filename) of
					{ok, []} ->
						SoFar;
					{ok, Records} ->
						Records ++ SoFar;
					{error, _Reason} ->
						%?log_debug("Missing msg stats report: ~p", [Filename])
						SoFar
				end
			end,
			[],
			Filenames),
	Report = status_stats_report(AllRecords, Status),
	{ok, Report}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec status_stats_report(Records::[#status_stats{}], Status::atom()) -> [tuple()].
status_stats_report(Records, undefined) ->
	Groups = k_statistic_utils:group(lists:sort(lists:map(
		fun(#status_stats{msg_status = #msg_status{status = Status}}) ->
			case Status of
				success_no_delivery -> sent;
				success_waiting_delivery -> sent_expected_delivery;
				Other -> Other
			end
		end,
		Records))),
	Freqs = lists:map(
		fun(Group = [Status|_]) ->
			{Status, length(Group)}
		end,
		Groups),
	{statuses, Freqs};

status_stats_report(Records, sent) ->
	status_stats_report(Records, success_no_delivery);
status_stats_report(Records, sent_expected_delivery) ->
	status_stats_report(Records, success_waiting_delivery);

status_stats_report(Records, Status) ->
	Filtered = lists:filter(
		fun(#status_stats{msg_status = #msg_status{status = St}}) when St =:= Status ->
			true;
		   (_) ->
			false
		end,
		Records),
	Report = lists:map(
		fun(#status_stats{
 				msg_info = #msg_info{
					id = MessageId,
					gateway_id = GatewayId,
					customer_id = CustomerId,
					src_addr = SrcAddr,
					dst_addr = DstAddr,
					body = BinText
				},
				time = Timestamp
			}) ->
			Datetime = k_statistic_utils:timestamp_to_iso_8601(Timestamp),
			[
				{datetime, Datetime},
				{message_id, MessageId},
				{gateway_id, GatewayId},
				{customer_id, CustomerId},
				{src_addr, transform_addr(SrcAddr)},
				{dst_addr, transform_addr(DstAddr)},
				{message_text, binary_to_list(BinText)}
			]
		end,
		Filtered),
	{messages, Report}.

transform_addr(#full_addr{
	addr = Addr,
	ton = Ton,
	npi = Npi
}) ->
	[
		{addr, Addr},
		{ton, Ton},
		{npi, Npi}
	];
transform_addr(#full_addr_ref_num{
	full_addr = FullAddr,
	ref_num = RefNum
}) ->
	transform_addr(FullAddr) ++ [{ref_num, RefNum}].
