-module(k_http_api_handler_uplink_stats).

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
        read = [],
        route = "/report/uplink"
    }}.

read(_Params) ->
    case k_support_just:get_throughput() of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Uplink report failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
