-module(k_prv_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_provider/2,
	get_provider/1,
	get_providers/0,
	del_provider/1
]).

-include_lib("k_common/include/provider.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_provider(provider_id(), #provider{}) -> ok | {error, term()}.
set_provider(ProviderId, Provider)->
	kv_storage_common:write_version(providers, ?CURRENT_VERSION, ProviderId, Provider).

-spec get_provider(provider_id()) -> {ok, #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
	kv_storage_common:read_version(providers, ?CURRENT_VERSION, ProviderId).

-spec get_providers() -> {ok, [{provider_id(), #provider{}}]} | {error, term()}.
get_providers() ->
	kv_storage_common:read_version(providers, ?CURRENT_VERSION).

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
	kv_storage_common:delete_version(providers, ?CURRENT_VERSION, ProviderId).
