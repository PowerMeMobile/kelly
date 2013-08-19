-module(k_statistic_status_reports).

-export([
	get_mt_msg_status_report/4,
	get_aggregated_statuses_report/2,
	get_msgs_by_status_report/3
]).

-include_lib("k_storage/include/msg_info.hrl").

-type report() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_mt_msg_status_report(
	customer_id(), user_id(), client_type(), in_msg_id()
) -> {ok, report()} | {error, reason()}.
get_mt_msg_status_report(CustomerId, UserId, ClientType, InMsgId) ->
	Selector = {
		'ci' , CustomerId,
		'ui' , UserId,
		'ct' , bsondoc:atom_to_binary(ClientType),
		'imi', InMsgId,
		'rqt', {'$exists', true}
	},
	case shifted_storage:find_one(mt_messages, Selector) of
		{ok, Doc} ->
			MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
			{ok, [
				{msg_id, MsgInfo#msg_info.msg_id},
				{client_type, ClientType},
				{customer_id, CustomerId},
				{user_id, UserId},
				{in_msg_id, InMsgId},
				{status, MsgInfo#msg_info.status}
			]};
		Error ->
			Error
	end.

-spec get_aggregated_statuses_report(timestamp(), timestamp()) ->
	{ok, report()} | {error, reason()}.
get_aggregated_statuses_report(From, To) ->
	MtMapF =
<<"
	function() {
		emit(this.s, 1);
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
	MtCommand = {
		'mapreduce' , <<"mt_messages">>,
		'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		'map' , MtMapF,
		'reduce' , ReduceF,
		'out' , { 'inline' , 1 }
	},
	MoCommand = {
		'mapreduce' , <<"mo_messages">>,
		'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		'map' , MoMapF,
		'reduce' , ReduceF,
		'out' , { 'inline' , 1 }
	},
	{ok, MtDocs} = shifted_storage:command(MtCommand),
	{ok, MoDocs} = shifted_storage:command(MoCommand),
	Docs = MtDocs ++ MoDocs,
	Results = merge([
		{list_to_existing_atom(binary_to_list(Status)), round(Hits)}
		|| {'_id', Status, value, Hits} <- Docs
	 ]),
	{ok, Results}.

merge(Pairs) ->
	dict:to_list(merge(Pairs, dict:new())).

merge([], Dict) ->
	Dict;
merge([{Key, Value}|Pairs], Dict) ->
	NewDict = dict:update_counter(Key, Value, Dict),
	merge(Pairs, NewDict).

-spec get_msgs_by_status_report(timestamp(), timestamp(), status()) ->
	{ok, report()} | {error, reason()}.
get_msgs_by_status_report(From, To, received) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		}
	},
	get_raw_report(mo_messages, Selector);

get_msgs_by_status_report(From, To, Status) when
	Status == pending;
	Status == sent; Status == failed;
	Status == enroute; Status == delivered; Status == expired;
	Status == deleted; Status == undeliverable; Status == accepted;
	Status == unknown; Status == rejected; Status == unrecognized
->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt' , To
		},
		's' , bsondoc:atom_to_binary(Status)
	},
	get_raw_report(mt_messages, Selector).

%% ===================================================================
%% Internal
%% ===================================================================

get_raw_report(Collection, Selector) ->
	case shifted_storage:find(Collection, Selector) of
		{ok, Docs} ->
			{ok, [doc_to_message(Collection, Doc) || {_Id, Doc} <- Docs]};
		Error ->
			Error
	end.

doc_to_message(mt_messages, Doc) ->
	MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
	Type = transform_type(MsgInfo#msg_info.type),
	ReqTime  = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
	RespTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.resp_time),
	DlrTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.dlr_time),
	StatusTime = max(ReqTime, max(RespTime, DlrTime)),
	ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
	StatusISO = ac_datetime:datetime_to_iso8601(StatusTime),
	[
		{msg_id, MsgInfo#msg_info.msg_id},
		{client_type, MsgInfo#msg_info.client_type},
		{customer_id, MsgInfo#msg_info.customer_id},
		{user_id, MsgInfo#msg_info.user_id},
		{in_msg_id, MsgInfo#msg_info.in_msg_id},
		{gateway_id, MsgInfo#msg_info.gateway_id},
		{out_msg_id, MsgInfo#msg_info.out_msg_id},
		{type, Type},
		{encoding, MsgInfo#msg_info.encoding},
		{body, MsgInfo#msg_info.body},
		{src_addr, addr_to_proplist(MsgInfo#msg_info.src_addr)},
		{dst_addr, addr_to_proplist(MsgInfo#msg_info.dst_addr)},
		{reg_dlr, MsgInfo#msg_info.reg_dlr},
		{esm_class, MsgInfo#msg_info.esm_class},
		{validity_period, MsgInfo#msg_info.val_period},
		{req_time, ReqISO},
		{status, MsgInfo#msg_info.status},
		{status_update_time, StatusISO}
	];
doc_to_message(mo_messages, Doc) ->
	MsgInfo = k_storage_utils:doc_to_mo_msg_info(Doc),
	MsgId = MsgInfo#msg_info.msg_id,
	Datetime  = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
	ISO8601 = ac_datetime:datetime_to_iso8601(Datetime),
	[
		{msg_id, MsgId},
		{customer_id, MsgInfo#msg_info.customer_id},
		{in_msg_id, MsgInfo#msg_info.in_msg_id},
		{gateway_id, MsgInfo#msg_info.gateway_id},
		{out_msg_id, MsgInfo#msg_info.out_msg_id},
		{type, MsgInfo#msg_info.type},
		{encoding, MsgInfo#msg_info.encoding},
		{body, MsgInfo#msg_info.body},
		{src_addr, addr_to_proplist(MsgInfo#msg_info.src_addr)},
		{dst_addr, addr_to_proplist(MsgInfo#msg_info.dst_addr)},
		{reg_dlr, MsgInfo#msg_info.reg_dlr},
		{req_time, ISO8601}
	].

addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
	[
		{addr, Addr},
		{ton, Ton},
		{npi, Npi}
	];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
	[
		{addr, Addr},
		{ton, Ton},
		{npi, Npi},
		{ref_num, RefNum}
	].

transform_type(regular) ->
	regular;
transform_type({part, #part_info{
	ref = PartRef,
	seq = PartSeq,
	total = TotalParts
}}) -> [
	{name, part},
	{ref, PartRef},
	{seq, PartSeq},
	{total, TotalParts}
].
