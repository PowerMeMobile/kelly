-module(k_config).

-export([
	set_network/2,
	del_network/1,
	get_network/1,
	get_networks/0,

	set_provider/2,
	del_provider/1,
	get_provider/1,
	get_providers/0,

	set_gateway/2,
	del_gateway/1,
	get_gateway/1,
	get_gateways/0,
	set_gateway_connection/2,
	del_gateway_connection/2
]).

-include("application.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% Networks API
%% ===================================================================

-spec set_network(network_id(), #network{}) -> ok | {error, term()}.
set_network(NetworkId, Network)->
	k_ntw_storage:set_network(NetworkId, Network).

-spec get_network(network_id()) -> {ok, #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
	k_ntw_storage:get_network(NetworkId).

-spec get_networks() -> {ok, [{network_id(), #network{}}]} | {error, term()}.
get_networks() ->
	k_ntw_storage:get_networks().

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
	k_ntw_storage:del_network(NetworkId).

%% ===================================================================
%% Providers API
%% ===================================================================

-spec set_provider(provider_id(), #provider{}) -> ok | {error, term()}.
set_provider(ProviderId, Provider)->
	k_prv_storage:set_provider(ProviderId, Provider).

-spec get_provider(provider_id()) -> {ok, #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
	k_prv_storage:get_provider(ProviderId).

-spec get_providers() -> {ok, [{provider_id(), #provider{}}]} | {error, term()}.
get_providers() ->
	k_prv_storage:get_providers().

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
	k_prv_storage:del_provider(ProviderId).

%% ===================================================================
%% Gateways API
%% ===================================================================

-spec set_gateway(gateway_id(), #gateway{}) -> ok | {error, term()}.
set_gateway(GatewayId, Gateway)->
	k_gtw_storage:set_gateway(GatewayId, Gateway).

-spec get_gateway(gateway_id()) -> {ok, #gateway{}} | {error, no_entry} | {error, term()}.
get_gateway(GatewayId) ->
	k_gtw_storage:get_gateway(GatewayId).

-spec get_gateways() -> {ok, [{gateway_id(), #gateway{}}]} | {error, term()}.
get_gateways() ->
	k_gtw_storage:get_gateways().

-spec del_gateway(gateway_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway(GatewayId) ->
	k_gtw_storage:del_gateway(GatewayId).

-spec set_gateway_connection(gateway_id(), #connection{}) -> ok | {error, no_entry} | {error, term()}.
set_gateway_connection(GatewayId, Connection = #connection{id = ConnId}) ->
	case get_gateway(GatewayId) of
		{ok, Gateway = #gateway{connections = Conns}} ->
			NewConns = delete_connection(Conns, ConnId),
			set_gateway(GatewayId, Gateway#gateway{connections = [Connection | NewConns]});
		Error ->
			 Error
	end.

-spec del_gateway_connection(gateway_id(), connection_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway_connection(GatewayId, ConnectionId) ->
	case get_gateway(GatewayId) of
		{ok, Gateway = #gateway{connections = Conns}} ->
			NewConns = delete_connection(Conns, ConnectionId),
			set_gateway(GatewayId, Gateway#gateway{connections = NewConns});
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

delete_connection(Conns, ConnId) ->
	lists:foldl(
		fun(CurrentConn = #connection{id = CurrentConnId}, Acc)->
			case CurrentConnId of
				ConnId -> Acc;
				_Any -> [CurrentConn | Acc]
			end
		end,
		[], Conns).
