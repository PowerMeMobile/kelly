-module(k_http_api_gui_index_router).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-spec init({term(), http}, cowboy_req:req(), []) ->
    {ok, cowboy_req:req(), undefined}.
init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

-spec handle(cowboy:req(), undefined) ->
    {ok, cowboy:req(), undefined}.
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, "/gui/Index.html"}], <<>>, Req),
    {ok, Req2, State}.

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
