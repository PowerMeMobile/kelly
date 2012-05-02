-module(k_http_api_handler_gui).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-record(state, {
	path :: list()
}).

%%% REST parameters
-record(get, {
}).

init(_Req, 'GET', Path) ->
	{ok, #get{}, #state{path = Path}};

init(_Req, HttpMethod, Path) ->
	?log_debug("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{path = Path}) ->
	{ok, ResponseBin} = get_file(Path),
	{ok, ResponseBin, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%%% Local functions

get_file([<<"gui">>]) ->
	Binary = <<"<HTML><HEAD><META http-equiv=\"refresh\" content=\"0; url=/gui/Index.html\"></HEAD></HTML>">>,
	{ok, Binary};
get_file(BinPath) ->
	Path = lists:map(fun(Elem)-> binary_to_list(Elem) end, BinPath),
	?log_debug("Path: ~p", [Path]),
	{ok, CWD} = file:get_cwd(),
	FileName = filename:join([CWD] ++ Path),
	?log_debug("FileName: ~p", [FileName]),
	read_file(FileName).

read_file(Filename) ->
	case file:read_file(Filename) of
		{ok, Binary} ->
			{ok, Binary};
		Error ->
			Error
	end.
