-module(k_http_api_v1_funnel_throughput).

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
    {ok, #specs{
        read = [],
        route = "/v1/funnel/throughput"
    }}.

read(_Params) ->
    case k_control_funnel:throughput() of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Get funnel throughput failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
