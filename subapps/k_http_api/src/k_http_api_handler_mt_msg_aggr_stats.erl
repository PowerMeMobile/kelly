-module(k_http_api_handler_mt_msg_aggr_stats).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
	Read = #method_spec{
				path = [<<"report">>, <<"mt_aggr">>],
				params = [
					#param{name = from, mandatory = true, repeated = false, type = {custom, fun convert_datetime/1}},
					#param{name = to, mandatory = true, repeated = false, type = {custom, fun convert_datetime/1}},
					#param{name = customer_id, mandatory = false, repeated = false, type = binary},
					#param{name = group_by , mandatory = true, repeated = false, type = {custom, fun convert_group_by/1}}
				]},

	{ok, #specs{
		create = undefined,
		read = Read,
		update = undefined,
		delete = undefined
	}}.

read(Params) ->
	{ok, k_statistic_mt_messages:build_aggr_report(Params)}.

create(_Params) ->
	ok.

update(_Params) ->
	ok.

delete(_Params) ->
	ok.

%% ===================================================================
%% Internal
%% ===================================================================

convert_group_by(<<"m">>) ->
	monthly;
convert_group_by(<<"d">>) ->
	daily;
convert_group_by(<<"h">>) ->
	hourly.

%% convert_datetime(<<"2012-12-11T13:20">>) => {{2012,12,11},{13,20,0}}.
-spec convert_datetime(binary()) -> calendar:datetime().
convert_datetime(DateTimeBin) ->
	DateTime = binary_to_list(DateTimeBin),
	DateTimeList = string:tokens(DateTime, [$-, $T, $:]),
	Result = [list_to_integer(List) || List <- DateTimeList],
	[Year, Month, Day, Hour, Minute] = Result,
	{{Year, Month, Day}, {Hour, Minute, 0}}.
