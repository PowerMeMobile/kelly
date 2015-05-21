-module(k_http_api_v1_defers).

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
-include_lib("alley_common/include/utils.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = user_id, mandatory = false, repeated = false, type = binary},
        #param{name = skip, mandatory = true, repeated = false, type = integer},
        #param{name = limit, mandatory = true, repeated = false, type = integer}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/defers"
    }}.

read(Params) ->
    {ok, Resp} = k_defers:get_all(Params),
    {ok, Resp}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
