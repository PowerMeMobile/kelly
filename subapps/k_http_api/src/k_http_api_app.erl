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
            {[<<"gateways">>, '_', <<"connections">>, '...'], gen_cowboy_crud, [k_http_api_handler_connections]},
            {[<<"gateways">>, '...'], gen_cowboy_crud, [k_http_api_handler_gateways]},
            {[<<"customers">>, '_', <<"users">>, '...'], gen_cowboy_crud, [k_http_api_handler_users]},
            {[<<"customers">>, '...'], gen_cowboy_crud, [k_http_api_handler_customers]},
            {[<<"networks">>, '...'], gen_cowboy_crud, [k_http_api_handler_networks]},
            {[<<"providers">>, '...'], gen_cowboy_crud, [k_http_api_handler_providers]},
			%% STATISTIC API
            {[<<"message_status">>, '_', <<"customer">>, '...'], gen_cowboy_crud, [k_http_api_handler_message_status]},
			{[<<"report">>, <<"uplink">>], gen_cowboy_crud, [k_http_api_handler_uplink_stats]},
			{[<<"report">>, <<"downlink">>], gen_cowboy_restful, [k_http_api_handler_downlink_stats]},
			{[<<"report">>, <<"statuses">>], gen_cowboy_restful, [k_http_api_handler_statuses_stats]},
            {[<<"report">>, <<"messages">>, '_'], gen_cowboy_restful, [k_http_api_handler_msg_stats]},
		    {[<<"addr2cust">>, '...'], gen_cowboy_crud, [k_http_api_handler_addr2cust]},
            {[<<"gui">>, '...'], gen_cowboy_restful, [k_http_api_handler_gui]},

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
