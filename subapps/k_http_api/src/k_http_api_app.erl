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
            {[<<"message_status">>, '_', <<"customer">>, '_'], gen_cowboy_restful, [k_http_api_handler_message_status]},
            {[<<"gateway">>, '_', <<"connection">>, '_'], gen_cowboy_restful, [k_http_api_handler_connections]},
            {[<<"gateway">>, '_', <<"connection">>], gen_cowboy_restful, [k_http_api_handler_connections]},
            {[<<"gateway">>, '_'], gen_cowboy_restful, [k_http_api_handler_gateways]},
            {[<<"gateway">>], gen_cowboy_restful, [k_http_api_handler_gateways]},
            {[<<"customer">>, '_', <<"user">>, '_'], gen_cowboy_restful, [k_http_api_handler_users]},
            {[<<"customer">>, '_', <<"user">>], gen_cowboy_restful, [k_http_api_handler_users]},
            {[<<"customer">>, '_'], gen_cowboy_restful, [k_http_api_handler_customers]},
            {[<<"customer">>], gen_cowboy_restful, [k_http_api_handler_customers]},
            {[<<"network">>, '_'], gen_cowboy_restful, [k_http_api_handler_networks]},
            {[<<"network">>], gen_cowboy_restful, [k_http_api_handler_networks]},
            {[<<"provider">>, '_'], gen_cowboy_restful, [k_http_api_handler_providers]},
            {[<<"provider">>], gen_cowboy_restful, [k_http_api_handler_providers]},
			{[<<"report">>, <<"gateways">>], gen_cowboy_restful, [k_http_api_handler_gtw_stats]},
            {[<<"report">>, '_'], gen_cowboy_restful, [k_http_api_handler_reports]},
		    {[<<"addr2cust">>], gen_cowboy_restful, [k_http_api_handler_addr2cust]},
            {[<<"gui">>, '...'], gen_cowboy_restful, [k_http_api_handler_gui]},
    		{'_', error_request_handler, []}
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
