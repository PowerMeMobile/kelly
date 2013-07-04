-module(error_request_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("k_common/include/logging.hrl").

-spec init({tcp, http}, term(), term()) -> {ok, term(), undefined_state}.
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [],
		<<"Error in your request">>, Req),
    ?log_debug("Error in request", []),
    ?log_debug("~p", [Req]),
    {ok, Req2, State}.

-spec terminate(term(), term()) -> ok.
terminate(_Req, _State) ->
    ok.
