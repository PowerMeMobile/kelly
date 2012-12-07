-module(k_gtw_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_gateway/2,
	get_gateway/1,
	get_gateways/0,
	del_gateway/1
]).

-include_lib("k_common/include/gateway.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_gateway(gateway_id(), #gateway{}) -> ok | {error, term()}.
set_gateway(GatewayId, Gateway)->
	kv_storage_common:write_version(gateways, ?CURRENT_VERSION, GatewayId, Gateway).

-spec get_gateway(gateway_id()) -> {ok, #gateway{}} | {error, no_entry} | {error, term()}.
get_gateway(GatewayId) ->
	kv_storage_common:read_version(gateways, ?CURRENT_VERSION, GatewayId).

-spec get_gateways() -> {ok, [{gateway_id(), #gateway{}}]} | {error, term()}.
get_gateways() ->
	kv_storage_common:read_version(gateways, ?CURRENT_VERSION).

-spec del_gateway(gateway_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway(GatewayId) ->
	kv_storage_common:delete_version(gateways, ?CURRENT_VERSION, GatewayId).
