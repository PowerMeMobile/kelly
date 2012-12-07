-module(k_http_api_handler_networks).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/network.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"networks">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = binary}]},
			#method_spec{
				path = [<<"networks">>],
				params = []}],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = name, mandatory = false, repeated = false, type = binary},
		#param{name = country_code, mandatory = false, repeated = false, type = binary},
		#param{name = numbers_len,	mandatory = false, repeated = false, type = integer},
		#param{name = prefixes, mandatory = false, repeated = true, type = binary},
		#param{name = provider_id, mandatory = false, repeated = false, type = binary}
	],
	Update = #method_spec{
				path = [<<"networks">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Delete = #method_spec{
				path = [<<"networks">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = binary},
		#param{name = name, mandatory = true, repeated = false, type = binary},
		#param{name = country_code, mandatory = true, repeated = false, type = binary},
		#param{name = numbers_len,	mandatory = true, repeated = false, type = integer},
		#param{name = prefixes, mandatory = true, repeated = true, type = binary},
		#param{name = provider_id, mandatory = true, repeated = false, type = binary}
	],
	Create = #method_spec{
				path = [<<"networks">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = Update,
			delete = Delete
		}}.

read(Params) ->
	NetworkUUID = ?gv(id, Params),
	case NetworkUUID of
		undefined ->
			read_all();
		_ -> read_id(NetworkUUID)
	end.

create(Params) ->
	case ?gv(id, Params) of
		undefined ->
			UUID = uuid:newid(),
			create_network(lists:keyreplace(id, 1, Params, {id, UUID}));
		_ ->
			is_exist(Params)
	end.

update(Params) ->
	ID = ?gv(id, Params),
	case k_config:get_network(ID) of
		{ok, Network = #network{}} ->
			update_network(Network, Params);
		{error, no_entry} ->
			{exception, 'svc0003'}
	end.

delete(Params) ->
	NetworkId = ?gv(id, Params),
	ok = k_config:del_network(NetworkId),
	{http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
	case k_config:get_networks() of
		{ok, NtwList} ->
			{ok, NtwPropLists} = prepare_ntws(NtwList),
			?log_debug("NtwPropLists: ~p", [NtwPropLists]),
			{http_code, 200, {networks, NtwPropLists}};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read_id(NtwUUID) ->
	case k_config:get_network(NtwUUID) of
		{ok, Ntw = #network{}} ->
			{ok, [NtwPropList]} = prepare_ntws({NtwUUID, Ntw}),
			?log_debug("NtwPropList: ~p", [NtwPropList]),
			{http_code, 200, NtwPropList};
		{error, no_entry} ->
			{exception, 'svc0003'}
	end.

is_exist(Params) ->
	UUID = ?gv(id, Params),
	case k_config:get_network(UUID) of
		{ok, #network{}} ->
			{exception, 'svc0004'};
		{error, no_entry} ->
			create_network(Params)
	end.

update_network(Network, Params) ->
	ID = ?gv(id, Params),
	NewName = ?resolve(name, Params, Network#network.name),
 	NewCountryCode = ?resolve(country_code, Params, Network#network.country_code),
	NewNumbersLen = ?resolve(numbers_len, Params, Network#network.numbers_len),
	NewPrefixes = ?resolve(prefixes, Params, Network#network.prefixes),
	NewProviderId = ?resolve(provider_id, Params, Network#network.provider_id),
	Updated = #network{
		name = NewName,
		country_code = NewCountryCode,
		numbers_len = NewNumbersLen,
		prefixes = NewPrefixes,
		provider_id = NewProviderId
	},
	ok = k_config:set_network(ID, Updated),
	{ok, [NtwPropList]} = prepare_ntws({ID, Updated}),
	?log_debug("NtwPropList: ~p", [NtwPropList]),
	{http_code, 200, NtwPropList}.

create_network(Params) ->
	ID = ?gv(id, Params),
	Name = ?gv(name, Params),
	CountryCode = ?gv(country_code, Params),
	NumbersLen = ?gv(numbers_len, Params),
	Prefixes = ?gv(prefixes, Params),
	ProviderId = ?gv(provider_id, Params),
 	Network = #network{
		name = Name,
		country_code = CountryCode,
		numbers_len = NumbersLen,
		prefixes = Prefixes,
		provider_id = ProviderId
	},
	ok = k_config:set_network(ID, Network),
	{ok, [NtwPropList]} = prepare_ntws({ID, Network}),
	?log_debug("NtwPropList: ~p", [NtwPropList]),
	{http_code, 201, NtwPropList}.


prepare_ntws(NtwList) when is_list(NtwList) ->
	prepare_ntws(NtwList, []);
prepare_ntws(Ntw = {_UUID, #network{}}) ->
	prepare_ntws([Ntw]).

prepare_ntws([], Acc) ->
	{ok, Acc};
prepare_ntws([{NtwUUID, Ntw = #network{}} | Rest], Acc) ->
	NtwFun = ?record_to_proplist(network),
	NtwPropList = NtwFun(Ntw),
	Result = translate([{id, list_to_binary(uuid:to_string(NtwUUID))}] ++ lists:keyreplace(providerId, 1, NtwPropList, {providerId, list_to_binary(uuid:to_string(?gv(provider_id, NtwPropList)))})),
	prepare_ntws(Rest, [Result | Acc]).

translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(provider_id) ->
	provider_id;
translate_name(numbers_len) ->
	numbers_len;
translate_name(country_code) ->
	country_code;
translate_name(Name) ->
	Name.
