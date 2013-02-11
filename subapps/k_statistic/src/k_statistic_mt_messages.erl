-module(k_statistic_mt_messages).

-export([
	build_report/1,
	build_aggr_report/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").

-define(gv(Key, List),
	proplists:get_value(Key, List)).

%% ===================================================================
%% API
%% ===================================================================

build_report(Params) ->
	From = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(?gv(from, Params))),
	To = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(?gv(to, Params))),
	CustomerSelector =
	case ?gv(customer_id, Params) of
		undefined -> { '$exists' , 1 };
		CustomerID -> CustomerID
	end,
	RecipientSelector =
	case ?gv(recipient, Params) of
		undefined -> { '$exists' , 1 };
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
	{ok, Docs} = mongodb_storage:find(mt_messages, Selector),
	[build_mt_report_response(Doc) || Doc <- Docs].

build_aggr_report(Params) ->
	From = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(?gv(from, Params))),
	To = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(?gv(to, Params))),
	CustomerSelector =
	case ?gv(customer_id, Params) of
		undefined -> { '$exists' , 1 };
		CustomerID -> CustomerID
	end,
	GroupBy = ?gv(group_by, Params),
	Command =
		{ 'aggregate', <<"mt_messages">>, 'pipeline', [
			{ '$match' , {
				'rqt' , {
					'$lt' , To,
					'$gt' , From },
				'ci' , CustomerSelector } },
			group(GroupBy),
			project(GroupBy)
	]},
	{ok,{result, Docs,ok,1.0}} = mongodb_storage:command(Command),
	[bson:fields(Doc) || Doc <- Docs].

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


build_mt_report_response({{ID}, Plist}) ->
	DstAddr = k_storage:doc_to_addr(proplists:get_value(da, Plist)),
	SrcAddr = k_storage:doc_to_addr(proplists:get_value(sa, Plist)),
	Timestamp = proplists:get_value(rqt, Plist),
	UnixEpoch  = k_datetime:timestamp_to_unix_epoch(Timestamp),
	Datetime = k_datetime:unix_epoch_to_datetime(UnixEpoch),
	ISO8601 = k_datetime:datetime_to_iso_8601(Datetime),
	[{message_id, base64:encode(ID)},
	{request_time, list_to_binary(ISO8601)},
	{customer_id, proplists:get_value(ci, Plist)},
	{client_type, proplists:get_value(ct, Plist)},
	{im_msg_id, proplists:get_value(imi, Plist)},
	{gateway_id, proplists:get_value(gi, Plist)},
	{type, proplists:get_value(t, Plist)},
	{encoding, proplists:get_value(e, Plist)},
	{body, proplists:get_value(b, Plist)},
	{src_addr, SrcAddr#addr.addr},
	{dst_addr, DstAddr#addr.addr},
	{reg_dlr, proplists:get_value(rd, Plist)},
	{out_msg_id, proplists:get_value(omi, Plist)},
	{status, get_status(Plist)}].

get_status(Plist) ->
	DeliveryStatus = proplists:get_value(ds, Plist),
	ResponseStatus = proplists:get_value(rps, Plist),
	get_status(DeliveryStatus, ResponseStatus).

%% see alley_dto hrl for available message statuses
%% haven't receipt or response - set status: pending
get_status(undefined, undefined) ->
	pending;
%% response status
get_status(undefined, success) ->
	sent;
get_status(undefined, failed) ->
	failed;
%% delivery status
get_status(State, _) ->
	State.
