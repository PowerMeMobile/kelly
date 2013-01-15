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
	Plist = [
		{name, Network#network.name},
		{country_code, Network#network.country_code},
		{numbers_len, Network#network.numbers_len},
		{prefixes, Network#network.prefixes},
		{provider_id, Network#network.provider_id}
	],
	mongodb_storage:upsert(?networkStorageName, [{'_id', NetworkId}], Plist).

-spec get_network(network_id()) -> {ok, #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
	case mongodb_storage:find_one(?networkStorageName, [{'_id', NetworkId}]) of
		{ok, Plist} when is_list(Plist) ->
			{ok, proplist_to_record(Plist)};
		Error ->
			Error
	end.

-spec get_networks() -> {ok, [{network_id(), #network{}}]} | {error, term()}.
get_networks() ->
	case mongodb_storage:find(?networkStorageName, []) of
		{ok, List} ->
			{ok, [
				{Id, proplist_to_record(Plist)} || {Id, Plist} <- List
			]};
		Error ->
			Error
	end.

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
	mongodb_storage:delete(?networkStorageName, [{'_id', NetworkId}]).

%% ===================================================================
%% Internals
%% ===================================================================

proplist_to_record(Plist) ->
	Name = proplists:get_value(name, Plist),
	CountryCode = proplists:get_value(country_code, Plist),
	NumbersLen = proplists:get_value(numbers_len, Plist),
	Prefixes = proplists:get_value(prefixes, Plist),
	ProviderId = proplists:get_value(provider_id, Plist),
	#network{
		name = Name,
		country_code = CountryCode,
		numbers_len = NumbersLen,
		prefixes = Prefixes,
		provider_id = ProviderId
	}.
