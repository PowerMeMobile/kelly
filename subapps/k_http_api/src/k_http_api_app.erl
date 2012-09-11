-module(k_http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("k_common/include/application_spec.hrl").
-include_lib("k_common/include/logging.hrl").

%% ==============================================================
%% Application callbacks
%% ==============================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	Dispatch = [
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
			%% STATISTIC API
            {[<<"message_status">>, '_', <<"customer">>, '...'], gen_http_api, [k_http_api_handler_message_status]},
			{[<<"report">>, <<"uplink">>], gen_http_api, [k_http_api_handler_uplink_stats]},
			{[<<"report">>, <<"downlink">>], gen_http_api, [k_http_api_handler_downlink_stats]},
			{[<<"report">>, <<"statuses">>], gen_http_api, [k_http_api_handler_statuses_stats]},
            {[<<"report">>, <<"messages">>, '_'], gen_http_api, [k_http_api_handler_msg_stats]},

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
	],
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	cowboy:start_listener(http, 1,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
	),
    % ?log_debug("cowboy started OK", []),
	k_http_api_sup:start_link().

stop(_State) ->
    ok.
