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
	Dispatch = dispatch_rules(),
	cowboy:start_listener(?MODULE, NumAcceptors,
	    cowboy_tcp_transport, [{port, Port}],
    	cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	k_http_api_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

update_dispatch_rules() ->
	cowboy:set_protocol_options(?MODULE, [{dispatch, dispatch_rules()}]).

%% ===================================================================
%% Internals
%% ===================================================================

dispatch_rules() ->
	[
    %% {Host, list({Path, Handler, Opts})}
    	{'_', [
			%% REST API
            {[<<"gateways">>, '_', <<"connections">>, '...'], gen_http_api, [k_http_api_handler_connections]},
            {[<<"gateways">>, '...'], gen_http_api, [k_http_api_handler_gateways]},
            {[<<"customers">>, '_', <<"users">>, '...'], gen_http_api, [k_http_api_handler_users]},
            {[<<"customers">>, '...'], gen_http_api, [k_http_api_handler_customers]},
            {[<<"networks">>, '...'], gen_http_api, [k_http_api_handler_networks]},
            {[<<"providers">>, '...'], gen_http_api, [k_http_api_handler_providers]},
		    {[<<"addr2cust">>, '...'], gen_http_api, [k_http_api_handler_addr2cust]},
			{[<<"just">>, <<"reconfigure">>], gen_http_api, [k_http_api_handler_just]},
			%% STATISTIC API
            {[<<"message_status">>, '_', <<"client">>, '_', <<"customer">>, '...'], gen_http_api, [k_http_api_handler_message_status]},
			{[<<"report">>, <<"uplink">>], gen_http_api, [k_http_api_handler_uplink_stats]},
			{[<<"report">>, <<"downlink">>], gen_http_api, [k_http_api_handler_downlink_stats]},
			{[<<"report">>, <<"statuses">>], gen_http_api, [k_http_api_handler_statuses_stats]},
            {[<<"report">>, <<"messages">>, '_'], gen_http_api, [k_http_api_handler_msg_stats]},
			{[<<"report">>, <<"mt_aggr">>], gen_http_api, [k_http_api_handler_mt_msg_aggr_stats]},
			{[<<"report">>, <<"mt">>], gen_http_api, [k_http_api_handler_mt_msg_stats]},

			%% GUI
            {[<<"gui">>], k_http_api_gui_index_router, []}, %% redirect to Index.html
			{[<<"gui">>, '...'], cowboy_http_static,
				[{directory, <<"./gui">>},
				{mimetypes, [
					{<<".css">>, [<<"text/css">>]},
					{<<".js">>, [<<"application/javascript">>]},
					{<<".html">>, [<<"text/html">>]}]}]},

			%% Others handler
    		{'...', error_request_handler, []}
    	]}
	].
