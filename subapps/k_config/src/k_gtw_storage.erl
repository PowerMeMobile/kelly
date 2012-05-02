-module(k_gtw_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_gateway/2,
	get_gateway/1,
	del_gateway/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_gateway(gateway_id(), #gateway{}) -> ok | {error, term()}.
set_gateway(GatewayId, Gateway)->
	k_gen_storage_common:write(gateways, ?CURRENT_VERSION, GatewayId, Gateway).

-spec get_gateway(gateway_id()) -> {ok, #gateway{}} | {error, no_entry} | {error, term()}.
get_gateway(GatewayId) ->
	k_gen_storage_common:read(gateways, ?CURRENT_VERSION, GatewayId).

-spec del_gateway(gateway_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway(GatewayId) ->
	k_gen_storage_common:delete(gateways, ?CURRENT_VERSION, GatewayId).
