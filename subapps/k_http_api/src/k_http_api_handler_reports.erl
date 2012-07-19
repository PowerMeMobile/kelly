-module(k_http_api_handler_reports).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-record(state, {
	type
}).

%%% REST parameters
-record(get, {
	from = {mandatory, <<"from">>, binary},
	to = {mandatory, <<"to">>, binary},
	slice_length = {optional, <<"slice_length">>, list}
}).

init(_Req, 'GET', [<<"report">>, <<"messages">>, <<"customers">>]) ->
	{ok, #get{}, #state{type = customers}};

init(_Req, 'GET', [<<"report">>, <<"messages">>, <<"networks">>]) ->
	{ok, #get{}, #state{type = networks}};

init(_Req, 'GET', [<<"report">>, <<"messages">>, <<"details">>]) ->
	{ok, #get{}, #state{type = details}};

init(_Req, HttpMethod, Path) ->
	?log_debug("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

%% format time: YYYY-MM-DDThh:mm
handle(_Req, #get{from = HttpFrom, to = HttpTo, slice_length = SliceLength}, State = #state{type = details}) ->
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
	SliceLengthSecs = convert_slice_length(SliceLength),
	{ok, Response} = k_statistic:detailed_msg_stats_report(From, To, SliceLengthSecs),
	{ok, Response, State};

handle(_Req, #get{from = HttpFrom, to = HttpTo}, State = #state{type = ReportType}) ->
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
	{ok, Response} = k_statistic:msg_stats_report(ReportType, From, To),
	{ok, Response, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%%% Local functions

-spec convert_http_datetime_to_term(string()) -> calendar:datetime().
convert_http_datetime_to_term(DateTime) ->
	DateTimeBinList = binary:split(DateTime, [<<"T">>, <<":">>, <<"-">>], [global]),
	Result =
	lists:map(fun(Bin)->
		list_to_integer(binary_to_list(Bin))
	end, DateTimeBinList),
	[Year, Month, Day, Hour, Minute] = Result,
	{{Year, Month, Day}, {Hour, Minute, 0}}.

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
