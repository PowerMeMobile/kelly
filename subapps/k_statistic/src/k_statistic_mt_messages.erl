-module(k_statistic_mt_messages).

-export([
	build_report/1,
	build_aggr_report/1,
	build_aggr_recipient_report/0
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_info.hrl").

-define(gv(Key, List),
	proplists:get_value(Key, List)).

%% ===================================================================
%% API
%% ===================================================================

build_report(Params) ->
	From = k_datetime:datetime_to_timestamp(?gv(from, Params)),
	To = k_datetime:datetime_to_timestamp(?gv(to, Params)),
	CustomerSelector =
	case ?gv(customer_id, Params) of
		undefined -> {'$exists', 1};
		CustomerID -> CustomerID
	end,
	RecipientSelector =
	case ?gv(recipient, Params) of
		undefined -> {'$exists', 1};
		Recipient -> Recipient
	end,
	StatusSelector =
	case ?gv(status, Params) of
		undefined -> [];
		pending -> [{'rps', {'$exists', false}}, {'ds', {'$exists', false}}];
		sent -> [{'rps', 'success'}, {'ds', {'$exists', false}}];
		failed -> [{'rps', 'failed'}, {'ds', {'$exists', false}}]
	end,

	Selector =
	[{'rqt', bson:document([
		{'$lt', To},
		{'$gt', From}])},
	{'ci', CustomerSelector},
	{'da.a', RecipientSelector}] ++ StatusSelector,
	{ok, Docs} = k_shifted_storage:find(mt_messages, bson:document(Selector)),
	[build_mt_report_response(Doc) || {_, Doc} <- Docs].

build_aggr_report(Params) ->
	From = k_datetime:datetime_to_timestamp(?gv(from, Params)),
	To = k_datetime:datetime_to_timestamp(?gv(to, Params)),
	CustomerSelector =
	case ?gv(customer_id, Params) of
		undefined -> {'$exists', 1};
		CustomerID -> CustomerID
	end,
	GroupBy = ?gv(group_by, Params),
	Command = {'aggregate', <<"mt_messages">>, 'pipeline', [
		{'$match', {
			'rqt', {'$lt', To, '$gt', From},
			'ci', CustomerSelector
		}},
		group(GroupBy),
		project(GroupBy)
	]},
	{ok, Docs} = k_shifted_storage:command(Command),
	SortedDocs = lists:sort(Docs),
	[bson:fields(Doc) || Doc <- SortedDocs].

build_aggr_recipient_report() ->
	Command = {'aggregate', <<"mt_messages">>, 'pipeline', [
		{'$project', {
			'_id', 0,
			'recipient', <<"$da.a">>
		}},
		{'$group', {
			'_id', <<"$recipient">>,
			'count', {<<"$sum">>, 1}
		}},
		{'$sort', {'count', 1}},
		{'$project', {
			'_id', 0,
			'recipient', <<"$_id">>,
			'count', 1
		}}
	]},
	{ok, _Docs} = k_shifted_storage:command(Command).

%% ===================================================================
%% Internals
%% ===================================================================

group(hourly) ->
	{ '$group' , {
		'_id' , {
			'year' , { '$year' , <<"$rqt">> },
			'month' , { '$month' , <<"$rqt">> },
			'day' , { '$dayOfMonth' , <<"$rqt">> },
			'hour' , { '$hour' , <<"$rqt">> },
			'customer_id' , <<"$ci">> },
		'sum' , { '$sum' , 1 } } };
group(daily) ->
	{ '$group' , {
		'_id' , {
			'year' , { '$year' , <<"$rqt">> },
			'month' , { '$month' , <<"$rqt">> },
			'day' , { '$dayOfMonth' , <<"$rqt">> },
			'customer_id' , <<"$ci">> },
		'sum' , { '$sum' , 1 } } };
group(monthly) ->
	{ '$group' , {
		'_id' , {
			'year' , { '$year' , <<"$rqt">> },
			'month' , { '$month' , <<"$rqt">> },
			'customer_id' , <<"$ci">> },
		'sum' , { '$sum' , 1 } } }.


project(hourly) ->
	{'$project' , {
		'_id' , 0,
		'year' , <<"$_id.year">>,
		'month' , <<"$_id.month">>,
		'day' , <<"$_id.day">>,
		'hour' , <<"$_id.hour">>,
		'customer_id' , <<"$_id.customer_id">>,
		'number' , <<"$sum">> } };
project(daily) ->
	{'$project' , {
		'_id' , 0,
		'year' , <<"$_id.year">>,
		'month' , <<"$_id.month">>,
		'day' , <<"$_id.day">>,
		'customer_id' , <<"$_id.customer_id">>,
		'number' , <<"$sum">> } };
project(monthly) ->
	{'$project' , {
		'_id' , 0,
		'year' , <<"$_id.year">>,
		'month' , <<"$_id.month">>,
		'customer_id' , <<"$_id.customer_id">>,
		'number' , <<"$sum">> } }.


build_mt_report_response(Doc) ->
	MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
	Datetime  = k_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
	ISO8601 = k_datetime:datetime_to_iso8601(Datetime),
	[
		{msg_id, MsgInfo#msg_info.msg_id},
		{client_type, MsgInfo#msg_info.client_type},
		{customer_id, MsgInfo#msg_info.customer_id},
		{user_id, MsgInfo#msg_info.user_id},
		{in_msg_id, MsgInfo#msg_info.in_msg_id},
		{gateway_id, MsgInfo#msg_info.gateway_id},
		{out_msg_id, MsgInfo#msg_info.out_msg_id},
		{type, MsgInfo#msg_info.type},
		{encoding, MsgInfo#msg_info.encoding},
		{body, MsgInfo#msg_info.body},
		{src_addr, MsgInfo#msg_info.src_addr#addr.addr},
		{dst_addr, MsgInfo#msg_info.dst_addr#addr.addr},
		{reg_dlr, MsgInfo#msg_info.reg_dlr},
		{req_time, list_to_binary(ISO8601)},
		{status, ?MSG_STATUS(MsgInfo)}
	].
