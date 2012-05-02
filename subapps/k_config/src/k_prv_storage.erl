-module(k_prv_storage).

-define(CURRENT_VERSION, 1).

%% API
-export([
	set_provider/2,
	del_provider/1,
	get_provider/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_provider(provider_id(), #provider{}) -> ok | {error, term()}.
set_provider(ProviderId, Provider)->
	k_gen_storage_common:write(providers, ?CURRENT_VERSION, ProviderId, Provider).

-spec get_provider(provider_id()) -> {ok, Provider :: #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
	k_gen_storage_common:read(providers, ?CURRENT_VERSION, ProviderId).

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
	k_gen_storage_common:delete(providers, ?CURRENT_VERSION, ProviderId).
