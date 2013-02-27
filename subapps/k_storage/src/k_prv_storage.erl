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
	Modifier = {
		'$set' , {
			'name'               , Provider#provider.name,
			'gateway'            , Provider#provider.gateway,
			'bulk_gateway'       , Provider#provider.bulk_gateway,
			'receipts_supported' , Provider#provider.receipts_supported
		}
	},
	mongodb_storage:upsert(k_static_storage, providers, {'_id', ProviderId}, Modifier).

-spec get_provider(provider_id()) -> {ok, #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
	case mongodb_storage:find_one(k_static_storage, providers, {'_id', ProviderId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec get_providers() -> {ok, [{provider_id(), #provider{}}]} | {error, term()}.
get_providers() ->
	case mongodb_storage:find(k_static_storage, providers, {}) of
		{ok, List} ->
			{ok, [
				{Id, doc_to_record(Doc)} || {Id, Doc} <- List
			]};
		Error ->
			Error
	end.

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
	mongodb_storage:delete(k_static_storage, providers, {'_id', ProviderId}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
	Name = bsondoc:at(name, Doc),
	Gtw = bsondoc:at(gateway, Doc),
	BulkGtw = bsondoc:at(bulk_gateway, Doc),
	ReceiptsSupported = bsondoc:at(receipts_supported, Doc),
 	#provider{
		name = Name,
		gateway = Gtw,
		bulk_gateway = BulkGtw,
		receipts_supported = ReceiptsSupported
	}.
