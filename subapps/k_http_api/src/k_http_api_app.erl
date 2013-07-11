-module(k_http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([update_dispatch_rules/0]).


-include("application.hrl").
-include_lib("k_common/include/application_spec.hrl").
-include_lib("k_common/include/logging.hrl").

%% ==============================================================
%% Application callbacks
%% ==============================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),

	{ok, NumAcceptors} = application:get_env(?APP, num_acceptors),
	{ok, Port} = application:get_env(?APP, port),
	ProtocolOpts = [
		{env, [{dispatch, dispatch_rules()}]}
	],
	{ok, _Pid} =
		cowboy:start_http(?MODULE, NumAcceptors, [{port, Port}], ProtocolOpts),
	k_http_api_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

-spec update_dispatch_rules() -> ok.
update_dispatch_rules() ->
	cowboy:set_env(?MODULE, dispatch, dispatch_rules()).

%% ===================================================================
%% Internals
%% ===================================================================

dispatch_rules() ->
	DispatchRaw = [
    %% {Host, list({Path, Handler, Opts})}
    	{'_', [
			%% REST API
			{"/gateways/:gateway_id/connections/[:connection_id]", gen_http_api, [k_http_api_handler_connections]},
			{"/gateways/[:gateway_id]", gen_http_api, [k_http_api_handler_gateways]},
			{"/customers/:customer_uuid/users/[:user_id]", gen_http_api, [k_http_api_handler_users]},
			{"/customers/[:customer_uuid]", gen_http_api, [k_http_api_handler_customers]},
			{"/networks/[:network_id]", gen_http_api, [k_http_api_handler_networks]},
			{"/providers/[:provider_id]", gen_http_api, [k_http_api_handler_providers]},
			{"/addr2cust/[:msisdn]", gen_http_api, [k_http_api_handler_addr2cust]},
			{"/just/reconfigure", gen_http_api, [k_http_api_handler_just]},
			%% STATISTIC API
			{"/message_status/:msg_id/client/:client_id/customer/:customer_id/user/:user_id",
				gen_http_api, [k_http_api_handler_message_status]},
			{"/report/uplink", gen_http_api, [k_http_api_handler_uplink_stats]},
			{"/report/downlink", gen_http_api, [k_http_api_handler_downlink_stats]},
			{"/report/statuses", gen_http_api, [k_http_api_handler_statuses_stats]},
			{"/report/messages/:type", gen_http_api, [k_http_api_handler_msg_stats]},
			{"/report/mt_aggr", gen_http_api, [k_http_api_handler_mt_msg_aggr_stats]},
			{"/report/mt", gen_http_api, [k_http_api_handler_mt_msg_stats]},
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
