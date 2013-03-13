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
	[build_mt_report_response(Doc) || Doc <- Docs].

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
	{ok, Docs} = k_shifted_storage:command(Command).

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


build_mt_report_response({{ID}, Doc}) ->
	DstAddr = k_storage_utils:doc_to_addr(bsondoc:at(da, Doc)),
	SrcAddr = k_storage_utils:doc_to_addr(bsondoc:at(sa, Doc)),
	Timestamp = bsondoc:at(rqt, Doc),
	UnixEpoch  = k_datetime:timestamp_to_unixepoch(Timestamp),
	Datetime = k_datetime:unixepoch_to_datetime(UnixEpoch),
	ISO8601 = k_datetime:datetime_to_iso8601(Datetime),
	[{message_id, lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(ID)])},
	{request_time, list_to_binary(ISO8601)},
	{customer_id, bsondoc:at(ci, Doc)},
	{client_type, bsondoc:at(ct, Doc)},
	{im_msg_id, bsondoc:at(imi, Doc)},
	{gateway_id, bsondoc:at(gi, Doc)},
	{type, bsondoc:at(t, Doc)},
	{encoding, bsondoc:at(e, Doc)},
	{body, bsondoc:at(b, Doc)},
	{src_addr, SrcAddr#addr.addr},
	{dst_addr, DstAddr#addr.addr},
	{reg_dlr, bsondoc:at(rd, Doc)},
	{out_msg_id, bsondoc:at(omi, Doc)},
	{status, get_status(Doc)}].

get_status(Doc) ->
	DeliveryStatus = bsondoc:at(ds, Doc),
	ResponseStatus = bsondoc:at(rps, Doc),
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
