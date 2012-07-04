-module(k_http_api_handler_downlink_stats).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
}).

%%% REST parameters
-record(get, {
}).

init(_Req, 'GET', [<<"report">>, <<"downlink">>]) ->
	{ok, #get{}, #state{}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{}) ->
	case k_downlink_stats:get_stats() of
		{ok, Report} ->
			{ok, Report, State};
		{error, Error} ->
			{ok, {error, io_lib:format("~p", [Error])}, State}
	end.

terminate(_Req, _State = #state{}) ->
    ok.
