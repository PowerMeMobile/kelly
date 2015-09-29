-module(k_http_api_v1_just_gateways).

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
        create = [
            #param{name = gateway_id, mandatory = true, repeated = false, type = uuid}
        ],
        delete = [
            #param{name = gateway_id, mandatory = true, repeated = false, type = uuid}
        ],
        route = "/v1/just/gateways/[:gateway_id]"
    }}.

read(_Params) ->
    case k_control_just:gateway_states() of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Get funnel connections failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(Params) ->
    GatewayId = ?gv(gateway_id, Params),
    case k_control_just:start_gateway(GatewayId) of
        ok ->
            {ok, <<>>};
        {error, Error} ->
            ?log_debug(
                "Just start gateway (gateway_id: ~p) failed with: ~p",
                [GatewayId, Error]),
            case Error of
                already_started ->
                    {exception, 'svc0004'};
                _ ->
                    {exception, 'svc0003'}
            end
    end.

update(_Params) ->
    ok.

delete(Params) ->
    GatewayId = ?gv(gateway_id, Params),
    case k_control_just:stop_gateway(GatewayId) of
        ok ->
            {ok, <<>>};
        {error, Error} ->
            ?log_debug(
                "Just stop gateway (gateway_id: ~p) failed with: ~p",
                [GatewayId, Error]),
            {exception, 'svc0003'}
    end.
