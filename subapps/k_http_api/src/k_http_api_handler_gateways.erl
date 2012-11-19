
-module(k_http_api_handler_gateways).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"gateways">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = binary}]},
			#method_spec{
				path = [<<"gateways">>],
				params = []}],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = name,	mandatory = false, repeated = false, type = binary},
		#param{name = rps, mandatory = false, repeated = false, type = integer}
	],
	Update = #method_spec{
				path = [<<"gateways">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Delete = #method_spec{
				path = [<<"gateways">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = binary},
		#param{name = name,	mandatory = true, repeated = false,	type = binary},
		#param{name = rps, mandatory = true, repeated = false, type = integer}
	],
	Create = #method_spec{
				path = [<<"gateways">>],
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
			UUID = uuid:newid(),
			create_gtw(lists:keyreplace(id, 1, Params, {id, UUID}));
		_Value ->
			is_exist(Params)
	end.

update(Params) ->
	UUID = ?gv(id, Params),
	case k_config:get_gateway(UUID) of
		{ok, Gtw = #gateway{}} ->
			update_gtw(Gtw, Params);
		{error, no_entry} ->
			{exception, 'svc0004'}
	end.

delete(Params) ->
	UUID = ?gv(id, Params),
	k_snmp:del_row(gtw, uuid:to_string(UUID)),
	ok = k_config:del_gateway(UUID),
	{http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
	case k_config:get_gateways() of
		{ok, GtwList} ->
			{ok, GtwPropLists} = prepare_gtws(GtwList),
			?log_debug("GtwPropLists: ~p", [GtwPropLists]),
			{ok, {gateways, GtwPropLists}};
		{error, no_entry} ->
			{exception, 'svc0003'}
   	end.

read_id(GtwUUID) ->
	case k_config:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{GtwUUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{ok, GtwPropList};
		{error, no_entry} ->
			{exception, 'svc0003'}
	end.

is_exist(Params) ->
	UUID = ?gv(id, Params),
	case k_config:get_gateway(UUID) of
		{ok, #gateway{}} ->
			?log_warn("Gateway already exist. Abort.", []),
			{exception, 'svc0004'};
		{error, no_entry} ->
			create_gtw(Params)
	end.

update_gtw(Gtw, Params) ->
	UUID = ?gv(id, Params),
	#gateway{rps = RPS, name = Name, connections = Conns} = Gtw,
	NewRPS = resolve(rps, Params, RPS),
	?log_debug("NewRPS: ~p", [NewRPS]),
	NewName = resolve(name, Params, Name),
	NewGtw = #gateway{rps = NewRPS, name = NewName, connections = Conns},
	?log_debug("New gtw: ~p", [NewGtw]),
	k_snmp:set_row(gtw, uuid:to_string(UUID), [{gtwName, binary_to_list(NewName)}, {gtwRPS, NewRPS}]),
	ok = k_config:set_gateway(UUID, NewGtw),
	case k_config:get_gateway(UUID) of
		{ok, NewGtw = #gateway{}} ->
			?log_debug("NewGtw: ~p", [NewGtw]),
			{ok, [GtwPropList]} = prepare_gtws([{UUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{http_code, 200, GtwPropList};
		{error, no_entry} ->
			?log_warn("Gateway not found after creation [~p]", [UUID]),
			{http_code, 500};
		Any ->
			?log_error("Unexpected error: ~p", [Any]),
			{http_code, 500}
	end.

create_gtw(Params) ->
	RPS = ?gv(rps, Params),
	Name = ?gv(name, Params),
	UUID = ?gv(id, Params),
	Gateway = #gateway{rps = RPS, name = Name},
	k_snmp:set_row(gtw, uuid:to_string(UUID), [{gtwName, binary_to_list(Name)}, {gtwRPS, RPS}]),
	ok = k_config:set_gateway(UUID, Gateway),
	case k_config:get_gateway(UUID) of
		{ok, Gtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{UUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{http_code, 201, GtwPropList};
		{error, no_entry} ->
			?log_warn("Gateway not found after creation [~p]", [UUID]),
			{http_code, 500};
		Any ->
			?log_error("Unexpected error: ~p", [Any]),
			{http_code, 500}
	end.

prepare_gtws(GtwList) when is_list(GtwList) ->
	prepare_gtws(GtwList, []);
prepare_gtws(Gtw = {_UUID, #gateway{}}) ->
	prepare_gtws([Gtw], []).

prepare_gtws([], Acc) ->
	{ok, Acc};
prepare_gtws([{GtwUUID, Gtw = #gateway{connections = Conns}} | Rest], Acc) ->

	%% convert connections records to proplists
	ConnFun = ?record_to_proplist(connection),
	ConnPropLists = [ConnFun(ConnRec) || ConnRec <- Conns],

	%% convert gateway record to proplist
	GatewayFun = ?record_to_proplist(gateway),
	GtwPropList =  [{id, list_to_binary(uuid:to_string(GtwUUID))}] ++ GatewayFun(Gtw#gateway{
													connections = ConnPropLists
													}),
	prepare_gtws(Rest, [GtwPropList | Acc]).

