-module(k_statistic_status_stats_report).

-export([
	get_report/2,
	get_report/3
]).

-include("status_stats.hrl").
-include("msg_stats.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(From::calendar:datetime(), To::calendar:datetime()) -> [any()].
get_report(From, To) ->
	FromDate = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(From)),
	ToDate = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(To)),
	MtMapF =
<<"
	function() {
		if (this.dlr_status) {
			emit(this.dlr_status, 1);
		} else if (this.resp_status) {
			emit(this.resp_status, 1);
		} else {
			emit(\"submitted\", 1);
		}
	};
">>,
	MoMapF =
<<"
	function() {
		emit(\"received\", 1);
	};
">>,
	ReduceF =
<<"
	function(key, values) {
		return Array.sum(values);
	};
">>,
	MtCommand =
		{ 'mapreduce' , <<"outgoing_messages">>,
		  'query' , { 'req_time' , { '$gte' , FromDate, '$lt' , ToDate } },
		  'map' , MtMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	MoCommand =
		{ 'mapreduce' , <<"incoming_messages">>,
		  'query' , { 'req_time' , { '$gte' , FromDate, '$lt' , ToDate } },
		  'map' , MoMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	{ok, MtBson} = mongodb_storage:command(outgoing_messages, MtCommand),
	{ok, MoBson} = mongodb_storage:command(incoming_messages, MoCommand),
	ResultsMtBson = bson:at(results, MtBson),
	ResultsMoBson = bson:at(results, MoBson),
	ResultsBson = lists:sort(ResultsMtBson ++ ResultsMoBson),
	Results = [
	 	{Status, round(Hits)} || {'_id', Status, value, Hits} <- ResultsBson
	 ],
	{ok, {statuses, Results}}.

-spec get_report(From::os:timestamp(), To::os:timestamp(), Status::atom()) -> [any()].
get_report(From, To, received) ->
	IncomingFilenames = k_statistic_utils:get_file_list_with(
		From, To, fun k_statistic_utils:incoming_msg_stats_slice_path/1),
	IncomingRecords = k_statistic_utils:read_terms_from_files(IncomingFilenames),
	IncomingReport = incoming_extended_report(IncomingRecords),
	{ok, {statuses, IncomingReport}};

get_report(From, To, Status) ->
	OutgoingFilenames = k_statistic_utils:get_file_list_with(
		From, To, fun k_statistic_utils:status_stats_slice_path/1),
	OutgoingRecords = k_statistic_utils:read_terms_from_files(OutgoingFilenames),
	OutgoingReport = outgoing_extended_report(OutgoingRecords, Status),

	{ok, {statuses, OutgoingReport}}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec merge_agregated_reports(OutgoingReport::[any()], IncomingReport::[any()]) -> [any()].
merge_agregated_reports(OutgoingReport, IncomingReport) ->
	lists:sort(
		fun({LStatus, _}, {RStatus, _}) ->
			LStatus =< RStatus
		end,
		OutgoingReport ++ IncomingReport).

-spec outgoing_agregated_report(Records::[#status_stats{}]) -> [{Status::atom(), Count::pos_integer()}].
outgoing_agregated_report(Records) ->
	Groups = k_statistic_utils:group(lists:sort(lists:map(
		fun(#status_stats{msg_status = #msg_status{status = Status}}) ->
			case Status of
				success_no_delivery -> sent;
				success_waiting_delivery -> sent_expected_delivery;
				Other -> Other
			end
		end,
		Records))),
	lists:map(
		fun(Group = [Status|_]) ->
			{Status, length(Group)}
		end,
		Groups).

-spec outgoing_extended_report(Records::[#status_stats{}], Status::atom()) -> [any()].
outgoing_extended_report(Records, sent) ->
	outgoing_extended_report(Records, success_no_delivery);
outgoing_extended_report(Records, sent_expected_delivery) ->
	outgoing_extended_report(Records, success_waiting_delivery);
outgoing_extended_report(Records, Status) ->
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
					in_msg_id = MessageId,
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
				{message_text, BinText}
			]
		end,
		Filtered),
	{messages, Report}.

-spec incoming_agregated_report(Records::[#msg_info{}]) -> [{Status::atom(), Count::pos_integer()}].
incoming_agregated_report([]) ->
	[];
incoming_agregated_report(Records) ->
	[{received, length(Records)}].

-spec incoming_extended_report(Records::[#status_stats{}]) -> [any()].
incoming_extended_report(Records) ->
	Report = lists:map(
		fun(#msg_stats{
			msg_info = #msg_info{
				in_msg_id = MessageId,
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
				{message_text, BinText}
			]
		end,
		Records),
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
