-module(k_http_api_handler_msg_stats).

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
				path = [<<"report">>, <<"messages">>, type],
				params = [
					#param{name = from, mandatory = true, repeated = false, type = string},
					#param{name = to, mandatory = true, repeated = false, type = string},
					#param{name = type, mandatory = true, repeated = false, type = string},
					#param{name = slice_length, mandatory = false, repeated = false, type = string}
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
	HttpType = ?gv(type, Params),
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
	Type = list_to_existing_atom(HttpType),
	case build_report(From, To, Type, Params) of
		{ok, Report} ->
			{ok, Report};
		{error, Error} ->
			?log_debug("Messages stats report failed with: ~p", [Error]),
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

build_report(From, To, customers, _Params) ->
	k_statistic:msg_stats_report(customers, From, To);

build_report(From, To, networks, _Params) ->
	k_statistic:msg_stats_report(networks, From, To);

build_report(From, To, details, Params) ->
	HttpSliceLength = ?gv(slice_length, Params),
	SliceLengthSecs = convert_slice_length(HttpSliceLength),
	k_statistic:detailed_msg_stats_report(From, To, SliceLengthSecs).

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

convert_slice_length([]) ->
	60;
convert_slice_length(undefined) ->
	60;
convert_slice_length("S" ++ Length) ->
	list_to_integer(Length);
convert_slice_length("M" ++ Length) ->
	60 * list_to_integer(Length);
convert_slice_length("H" ++ Length) ->
	60 * 60 * list_to_integer(Length);
convert_slice_length(Length) ->
	list_to_integer(Length).
