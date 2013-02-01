-module(k_statistic_mt_messages).

-export([
	build_aggr_report/1
]).

-include_lib("k_common/include/logging.hrl").

-define(gv(Key, List),
	proplists:get_value(Key, List)).

%% ===================================================================
%% API
%% ===================================================================

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
	?log_debug("Command: ~p", [Command]),
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
