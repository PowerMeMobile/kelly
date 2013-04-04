-module(k_http_api_handler_providers).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/provider.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"providers">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = binary}]},
			#method_spec{
				path = [<<"providers">>],
				params = []}
			],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = name, mandatory = false, repeated = false, type = binary},
		#param{name = gateway, mandatory = false, repeated = false, type = binary},
		#param{name = bulk_gateway,	mandatory = false, repeated = false, type = binary},
		#param{name = receipts_supported, mandatory = false, repeated = false, type = boolean}
	],
	Update = #method_spec{
				path = [<<"providers">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Delete = #method_spec{
				path = [<<"providers">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = binary},
		#param{name = name, mandatory = true, repeated = false, type = binary},
		#param{name = gateway, mandatory = true, repeated = false, type = binary},
		#param{name = bulk_gateway, mandatory = true, repeated = false, type = binary},
		#param{name = receipts_supported, mandatory = true, repeated = false, type = boolean}
	],
	Create = #method_spec{
				path = [<<"providers">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = Update,
			delete = Delete
		}}.

read(Params) ->
	UUID = ?gv(id, Params),
	case UUID of
		undefined -> read_all();
		_ -> read_id(UUID)
	end.

create(Params) ->
	case ?gv(id, Params) of
		undefined ->
			UUID = uuid:unparse(uuid:generate_time()),
			create_provider(lists:keyreplace(id, 1, Params, {id, UUID}));
		_ ->
			is_exist(Params)
	end.

update(Params) ->
	case k_config:get_provider(?gv(id, Params)) of
		{ok, Provider = #provider{}} ->
			update_provider(Provider, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

delete(Params) ->
	ok = k_config:del_provider(?gv(id, Params)),
	{http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

is_exist(Params) ->
	case k_config:get_provider(?gv(id, Params)) of
		{ok, #provider{}} ->
			{exception, 'svc0004'};
		{error, no_entry} ->
			create_provider(Params);
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read_all() ->
	case k_config:get_providers() of
		{ok, PrvList} ->
			{ok, PrvPropLists} = prepare(PrvList),
			?log_debug("PrvPropLists: ~p", [PrvPropLists]),
			{http_code, 200, {providers, PrvPropLists}};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read_id(PrvUUID) ->
	case k_config:get_provider(PrvUUID) of
		{ok, Prv = #provider{}} ->
			{ok, [PrvPropList]} = prepare({PrvUUID, Prv}),
			?log_debug("PrvPropList: ~p", [PrvPropList]),
			{http_code, 200, PrvPropList};
		{error, no_entry} ->
			{exception, 'svc0003', []};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

update_provider(Provider, Params) ->
	ID = ?gv(id, Params),
	Name = ?resolve(name, Params, Provider#provider.name),
	Gateway = ?resolve(gateway, Params, Provider#provider.gateway),
	BulkGateway = ?resolve(bulk_gateway, Params, Provider#provider.bulk_gateway),
	ReceiptsSupported = ?resolve(receipts_supported, Params, Provider#provider.receipts_supported),
	Updated = #provider{
		name = Name,
		gateway = Gateway,
		bulk_gateway = BulkGateway,
		receipts_supported = ReceiptsSupported},
	ok = k_config:set_provider(ID, Updated),
	{ok, [PrvPropList]} = prepare({ID, Updated}),
	?log_debug("PrvPropList: ~p", [PrvPropList]),
	{http_code, 200, PrvPropList}.

create_provider(Params) ->
	UUID = ?gv(id, Params),
	Name = ?gv(name, Params),
	Gateway = ?gv(gateway, Params),
	BulkGateway = ?gv(bulk_gateway, Params),
	ReceiptsSupported = ?gv(receipts_supported, Params),
	Provider = #provider{
		name = Name,
		gateway = Gateway,
		bulk_gateway = BulkGateway,
		receipts_supported = ReceiptsSupported
	},
	ok = k_config:set_provider(UUID, Provider),
	{ok, [PrvPropList]} = prepare({UUID, Provider}),
	?log_debug("PrvPropList: ~p", [PrvPropList]),
	{http_code, 201, PrvPropList}.

prepare(PrvList) when is_list(PrvList) ->
	prepare(PrvList, []);
prepare(Prv = {_UUID, #provider{}}) ->
	prepare([Prv]).

prepare([], Acc) ->
	{ok, Acc};
prepare([{PrvUUID, Prv = #provider{}} | Rest], Acc) ->
	PrvFun = ?record_to_proplist(provider),
	PrvPropList = PrvFun(Prv),
	Name = ?gv(name, PrvPropList),
	BulkGateway = ?gv(bulk_gateway, PrvPropList),
	Gateway = ?gv(gateway, PrvPropList),
	ReceiptsSupported = ?gv(receipts_supported, PrvPropList),
	Result = translate([{id, PrvUUID}] ++ [{name, Name}] ++ [{gateway, Gateway}] ++ [{bulk_gateway, BulkGateway}] ++ [{receipts_supported, ReceiptsSupported}]),
	prepare(Rest, [Result | Acc]).



translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(bulk_gateway) ->
	bulk_gateway;
translate_name(receipts_supported) ->
	receipts_supported;
translate_name(Name) ->
	Name.
