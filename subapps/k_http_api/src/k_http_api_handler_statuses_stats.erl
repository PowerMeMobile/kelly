-module(k_http_api_handler_statuses_stats).

-behaviour(gen_cowboy_crud).

%% gen_cowboy_crud callbacks
-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include("crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
	Read = #method_spec{
				path = [<<"report">>, <<"statuses">>],
				params = [
					#param{name = from, mandatory = true, repeated = false, type = string},
					#param{name = to, mandatory = true, repeated = false, type = string},
					#param{name = status, mandatory = false, repeated = false, type = string}
				]},

	{ok, #specs{
		create = undefined,
		read = Read,
		update = undefined,
		delete = undefined
	}}.

read(Params) ->
	?log_debug("Params: ~p", [Params]),
	HttpFrom = ?gv(from, Params),
	HttpTo = ?gv(to, Params),
	HttpStatus = ?gv(status, Params),
	case build_report(HttpFrom, HttpTo, HttpStatus) of
		{ok, Report} ->
			{ok, Report};
		{error, Error} ->
			?log_debug("Statuses stats report failed with: ~p", [Error]),
			{exception, 'svc0003'}
	end.

create(_Params) ->
	ok.

update(_Params) ->
	ok.

delete(_Params) ->
	ok.

%% ===================================================================
%% Internal
%% ===================================================================

build_report(HttpFrom, HttpTo, undefined) ->
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
	k_statistic:status_stats_report(From, To);

build_report(HttpFrom, HttpTo, HttpStatus) ->
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
 	Status = list_to_existing_atom(HttpStatus),
 	k_statistic:status_stats_report(From, To, Status).

-spec convert_http_datetime_to_term(string()) -> calendar:datetime().
convert_http_datetime_to_term(DateTime) ->
	DateTimeList = string:tokens(DateTime, [$T, $:, $-]),
	Result = lists:map(
		fun(List)->
			list_to_integer(List)
		end,
		DateTimeList),
	[Year, Month, Day, Hour, Minute] = Result,
	{{Year, Month, Day}, {Hour, Minute, 0}}.
