-module(k_prv_storage).

%% API
-export([
	set_provider/2,
	get_provider/1,
	get_providers/0,
	del_provider/1
]).

-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/provider.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_provider(provider_id(), #provider{}) -> ok | {error, term()}.
set_provider(ProviderId, Provider)->
	Plist = [
		{name, Provider#provider.name},
		{gateway, Provider#provider.gateway},
		{bulk_gateway, Provider#provider.bulk_gateway},
		{receipts_supported, Provider#provider.receipts_supported}
	],
	mongodb_storage:upsert(?providerStorageName, [{'_id', ProviderId}], Plist).

-spec get_provider(provider_id()) -> {ok, #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
	case mongodb_storage:find_one(?providerStorageName, [{'_id', ProviderId}]) of
		{ok, Plist} when is_list(Plist) ->
			{ok, proplist_to_record(Plist)};
		Error ->
			Error
	end.

-spec get_providers() -> {ok, [{provider_id(), #provider{}}]} | {error, term()}.
get_providers() ->
	case mongodb_storage:find(?providerStorageName, []) of
		{ok, List} ->
			{ok, [
				{Id, proplist_to_record(Plist)} || {Id, Plist} <- List
			]};
		Error ->
			Error
	end.

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
	mongodb_storage:delete(?providerStorageName, [{'_id', ProviderId}]).

%% ===================================================================
%% Internals
%% ===================================================================

proplist_to_record(Plist) ->
	Name = proplists:get_value(name, Plist),
	Gtw = proplists:get_value(gateway, Plist),
	BulkGtw = proplists:get_value(bulk_gateway, Plist),
	ReceiptsSupported = proplists:get_value(receipts_supported, Plist),
 	#provider{
		name = Name,
		gateway = Gtw,
		bulk_gateway = BulkGtw,
		receipts_supported = ReceiptsSupported
	}.
