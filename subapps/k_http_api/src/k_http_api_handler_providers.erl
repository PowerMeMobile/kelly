-module(k_http_api_handler_providers).

-behaviour(gen_cowboy_crud).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("crud_specs.hrl").

%% ===================================================================
%% API Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"providers">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = string_uuid}]},
			#method_spec{
				path = [<<"providers">>],
				params = []}
			],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = gateway, mandatory = false, repeated = false, type = string_uuid},
		#param{name = bulk_gateway,	mandatory = false, repeated = false, type = string_uuid},
		#param{name = receipts_supported, mandatory = fasle, repeated = false, type = boolean}
	],
	Update = #method_spec{
				path = [<<"providers">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = string_uuid}
	],
	Delete = #method_spec{
				path = [<<"providers">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = string_uuid},
		#param{name = gateway, mandatory = true, repeated = false, type = string_uuid},
		#param{name = bulk_gateway, mandatory = true, repeated = false, type = string_uuid},
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
			UUID = k_uuid:to_string(k_uuid:newid()),
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
%% Local Functions Definitions
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
	Gateway = resolve(gateway, Params, Provider#provider.gateway),
	BulkGateway = resolve(bulk_gateway, Params, Provider#provider.bulkGateway),
	ReceiptsSupported = resolve(receipts_supported, Params, Provider#provider.receiptsSupported),
	Updated = #provider{
		gateway = Gateway,
		bulkGateway = BulkGateway,
		receiptsSupported = ReceiptsSupported},
	ok = k_config:set_provider(ID, Updated),
	{ok, [PrvPropList]} = prepare({ID, Updated}),
	?log_debug("PrvPropList: ~p", [PrvPropList]),
	{http_code, 200, PrvPropList}.

create_provider(Params) ->
	UUID = ?gv(id, Params),
	Gateway = ?gv(gateway, Params),
	BulkGateway = ?gv(bulk_gateway, Params),
	ReceiptsSupported = ?gv(receipts_supported, Params),
	Provider = #provider{
		gateway = Gateway,
		bulkGateway = BulkGateway,
		receiptsSupported = ReceiptsSupported
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
	PrvPropList = [{id, PrvUUID}] ++ PrvFun(Prv),
	prepare(Rest, [PrvPropList | Acc]).


