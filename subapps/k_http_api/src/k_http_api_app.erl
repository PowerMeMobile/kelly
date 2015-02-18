-module(k_http_api_app).

-behaviour(application).

-on_load(try_update_dispatch_rules/0).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([update_dispatch_rules/0]).

-include("application.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/application_spec.hrl").

%% ==============================================================
%% Application callbacks
%% ==============================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),

    {ok, Addr} = application:get_env(?APP, addr),
    {ok, Port} = application:get_env(?APP, port),
    {ok, AcceptorsNum} = application:get_env(?APP, acceptors_num),

    TransOpts = [{ip, Addr}, {port, Port}],
    ProtoOpts = [
        {env, [{dispatch, dispatch_rules()}]}
    ],
    {ok, _Pid} =
        cowboy:start_http(?MODULE, AcceptorsNum, TransOpts, ProtoOpts),

    k_http_api_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

-spec update_dispatch_rules() -> ok.
update_dispatch_rules() ->
    try
        cowboy:set_env(?MODULE, dispatch, dispatch_rules()),
        ?log_info("Dispatch rules updated successfully", [])
    catch
        Class:Error ->
            ?log_warn("Failed to update dispatch rules (~p:~p)",
                [Class, Error])
    end.

%% on load try to update dispatch rules automatically
try_update_dispatch_rules() ->
    WhichApps = application:which_applications(1000),
    case lists:keymember(?APP, 1, WhichApps) of
        true ->
            update_dispatch_rules(),ok;
        false -> ok
    end.

%% ===================================================================
%% Internals
%% ===================================================================

dispatch_rules() ->
    DispatchRaw = [
    %% {Host, list({Path, Handler, Opts})}
        {'_', gen_http_api_handlers_dispatch_rules() ++ [
            %% GUI
            {"/gui", k_http_api_gui_index_router, []}, %% redirect to Index.html
            {"/gui/[...]", cowboy_static, [
                {directory, <<"gui">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]},
            %% Error(404) handler
            {'_', k_http_api_not_found_handler, []}
        ]}
    ],
    cowboy_router:compile(DispatchRaw).

gen_http_api_handlers_dispatch_rules() ->
    gen_http_api:compile_routes([
        %% REST API
        k_http_api_handler_gateways,
        k_http_api_handler_gateway_settings,
        k_http_api_handler_connections,
        k_http_api_handler_providers,
        k_http_api_handler_networks,
        k_http_api_handler_network_maps,
        k_http_api_handler_users,
        k_http_api_handler_users_features,
        k_http_api_handler_customers,
        k_http_api_handler_customers_credit,
        k_http_api_handler_customers_originators,
        k_http_api_handler_blacklist,
        k_http_api_handler_addr2cust,
        k_http_api_handler_just_reconfigure,

        %% Statistic API
        k_http_api_handler_message_status,
        k_http_api_handler_uplink_stats,
        k_http_api_handler_downlink_stats,
        k_http_api_handler_statuses_stats,
        k_http_api_handler_msg_stats,
        k_http_api_handler_mt_msg_aggr_stats,
        k_http_api_handler_mt_msg_stats,
        k_http_api_handler_mt_msg
    ]).
