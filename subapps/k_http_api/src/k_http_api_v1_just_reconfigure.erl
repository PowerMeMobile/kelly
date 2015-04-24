-module(k_http_api_v1_just_reconfigure).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    {ok, #specs{
        create = undefined,
        read = [],
        update = undefined,
        delete = undefined,
        route = "/v1/just/reconfigure"
    }}.

read(_Params) ->
    ok = k_support_just:reconfigure(),
    {ok, {result, ok}}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
