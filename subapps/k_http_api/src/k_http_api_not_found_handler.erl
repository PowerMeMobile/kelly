-module(k_http_api_not_found_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include_lib("k_common/include/logging.hrl").

-spec init({tcp, http}, cowboy_req:req(), term()) ->
	{ok, cowboy_req:req(), undefined_state}.
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

-spec handle(cowboy_req:req(), term()) ->
	{ok, cowboy_req:req(), term()}.
handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(404, [],
		<<"Resource not found">>, Req),
    ?log_debug("Error in request", []),
    ?log_debug("~p", [Req]),
    {ok, Req2, State}.

-spec terminate(any(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
