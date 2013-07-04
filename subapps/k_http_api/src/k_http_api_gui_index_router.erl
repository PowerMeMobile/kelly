-module(k_http_api_gui_index_router).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-spec init({term(), http}, term(), []) -> {ok, term(), undefined}.
init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

-spec handle(term(), undefined) -> {ok, term(), undefined}.
handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(302, [{'Location', "/gui/Index.html"}], <<>>, Req),
    {ok, Req2, State}.

-spec terminate(term(), term()) -> ok.
terminate(_Req, _State) ->
	ok.
