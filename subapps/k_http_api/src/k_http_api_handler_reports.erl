-module(k_http_api_handler_reports).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-record(state, {
	id
}).

%%% REST parameters
-record(get, {
	from = {mandatory, <<"from">>, binary},
	to = {mandatory, <<"to">>, binary}
}).

init(_Req, 'GET', [<<"report">>, BinId]) ->
	Id = list_to_integer(binary_to_list(BinId)),
	{ok, #get{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_debug("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

%% format time: YYYY-MM-DDThh:mm
handle(_Req, #get{from = HttpFrom, to = HttpTo}, State = #state{id = ReportId}) ->
	From = convert_http_datetime_to_term(HttpFrom),
	To = convert_http_datetime_to_term(HttpTo),
	{ok, Response} = k_statistic:msg_stats_report(ReportId, From, To),
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
