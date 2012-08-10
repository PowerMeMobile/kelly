-module(k_ntw_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_network/2,
	get_network/1,
	get_networks/0,
	del_network/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_network(network_id(), #network{}) -> ok | {error, term()}.
set_network(NetworkId, Network)->
	kv_storage_common:write_version(networks, ?CURRENT_VERSION, NetworkId, Network).

-spec get_network(network_id()) -> {ok, #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
	kv_storage_common:read_version(networks, ?CURRENT_VERSION, NetworkId).

-spec get_networks() -> {ok, [{network_id(), #network{}}]} | {error, term()}.
get_networks() ->
	kv_storage_common:read_version(networks, ?CURRENT_VERSION).

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
	kv_storage_common:delete_version(networks, ?CURRENT_VERSION, NetworkId).
