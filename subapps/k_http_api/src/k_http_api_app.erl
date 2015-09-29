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
            update_dispatch_rules();
        false ->
            ok
    end.

%% ===================================================================
%% Internals
%% ===================================================================

dispatch_rules() ->
    DispatchRaw = [
    %% {Host, list({Path, Handler, Opts})}
        {'_', gen_http_api_handlers_dispatch_rules() ++ [
            %% Error(404) handler
            {'_', k_http_api_not_found_handler, []}
        ]}
    ],
    cowboy_router:compile(DispatchRaw).

gen_http_api_handlers_dispatch_rules() ->
    gen_http_api:compile_routes([
        %% deprecated since PowerAlley UI 1.6.0
        k_http_api_v1_blacklists,

        %% Config API
        k_http_api_handler_gateways,
        k_http_api_handler_gateways_connections,
        k_http_api_handler_gateways_settings,
        k_http_api_handler_providers,
        k_http_api_handler_networks,
        k_http_api_handler_network_maps,

        %% Statistic API
        k_http_api_handler_statuses_stats,
        k_http_api_handler_msg_stats,
        k_http_api_handler_mt_msg_stats,
        k_http_api_handler_mt_msg,

        %%
        %% API v1
        %%

        %% Control API
        k_http_api_v1_just_gateways,
        k_http_api_v1_just_reconfigure,
        k_http_api_v1_just_throughput,
        k_http_api_v1_funnel_connections,
        k_http_api_v1_funnel_throughput,

        %% Config API
        k_http_api_v1_blacklist,
        k_http_api_v1_customers_credit,
        k_http_api_v1_customers_msisdns,
        k_http_api_v1_customers_originators,
        k_http_api_v1_customers_users,
        k_http_api_v1_customers_users_msisdns,
        k_http_api_v1_customers,

        %% Inbox API
        k_http_api_v1_incomings,
        k_http_api_v1_msisdns,

        %% Batches API
        % keep batches details above batches
        k_http_api_v1_batches_details,
        k_http_api_v1_batches,
        k_http_api_v1_batches_block,
        k_http_api_v1_batches_recipients,

        %% Defers API
        % keep defers details above defers
        k_http_api_v1_defers_details,
        k_http_api_v1_defers,
        k_http_api_v1_defers_recipients,

        %% Reports API
        k_http_api_v1_reports_mt_aggr_by_country,
        k_http_api_v1_reports_mt_aggr_by_country_and_network,
        k_http_api_v1_reports_mt_aggr_by_gateway,
        k_http_api_v1_reports_mt_aggr_by_period,
        k_http_api_v1_reports_mt_aggr_summary
    ]).
