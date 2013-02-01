-module(k_ntw_storage).

%% API
-export([
	set_network/2,
	get_network/1,
	get_networks/0,
	del_network/1
]).

-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/network.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_network(network_id(), #network{}) -> ok | {error, term()}.
set_network(NetworkId, Network)->
	Modifier = {
		'$set' , {
			'name'         , Network#network.name,
			'country_code' , Network#network.country_code,
			'numbers_len'  , Network#network.numbers_len,
			'prefixes'     , Network#network.prefixes,
			'provider_id'  , Network#network.provider_id
		}
	},
	mongodb_storage:upsert(k_static_storage, networks, {'_id', NetworkId}, Modifier).

-spec get_network(network_id()) -> {ok, #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
	case mongodb_storage:find_one(k_static_storage, networks, {'_id', NetworkId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec get_networks() -> {ok, [{network_id(), #network{}}]} | {error, term()}.
get_networks() ->
	case mongodb_storage:find(k_static_storage, networks, {}) of
		{ok, List} ->
			{ok, [
				{Id, doc_to_record(Doc)} || {Id, Doc} <- List
			]};
		Error ->
			Error
	end.

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
	mongodb_storage:delete(k_static_storage, networks, {'_id', NetworkId}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
	Name = bson:at(name, Doc),
	CountryCode = bson:at(country_code, Doc),
	NumbersLen = bson:at(numbers_len, Doc),
	Prefixes = bson:at(prefixes, Doc),
	ProviderId = bson:at(provider_id, Doc),
	#network{
		name = Name,
		country_code = CountryCode,
		numbers_len = NumbersLen,
		prefixes = Prefixes,
		provider_id = ProviderId
	}.
