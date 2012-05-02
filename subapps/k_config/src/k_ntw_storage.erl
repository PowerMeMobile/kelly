-module(k_ntw_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_network/2,
	del_network/1,
	get_network/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_network(network_id(), #network{}) -> ok | {error, term()}.
set_network(NetworkId, Network)->
	k_gen_storage_common:write(networks, ?CURRENT_VERSION, NetworkId, Network).

-spec get_network(network_id()) -> {ok, Network :: #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
	k_gen_storage_common:read(networks, ?CURRENT_VERSION, NetworkId).

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
	k_gen_storage_common:delete(networks, ?CURRENT_VERSION, NetworkId).
