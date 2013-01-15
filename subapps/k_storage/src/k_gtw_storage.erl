-module(k_gtw_storage).

%% API
-export([
	set_gateway/2,
	get_gateway/1,
	get_gateways/0,
	del_gateway/1
]).

-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/gateway.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_gateway(gateway_id(), #gateway{}) -> ok | {error, term()}.
set_gateway(GatewayId, Gateway)->
	ConnectionsPList =
	[	bson:document(
		[{id, Conn#connection.id},
		{type, Conn#connection.type},
		{addr, Conn#connection.addr},
		{port, Conn#connection.port},
		{sys_id, Conn#connection.sys_id},
		{pass, Conn#connection.pass},
		{sys_type, Conn#connection.pass},
		{addr_ton, Conn#connection.addr_ton},
		{addr_npi, Conn#connection.addr_npi},
		{addr_range, Conn#connection.addr_range}]) || Conn <- Gateway#gateway.connections],
	Plist = [
		{name, Gateway#gateway.name},
		{rps, Gateway#gateway.rps},
		{connections, ConnectionsPList}
	],
	mongodb_storage:upsert(?gatewayStorageName, [{'_id', GatewayId}], Plist).

-spec get_gateway(gateway_id()) -> {ok, #gateway{}} | {error, no_entry} | {error, term()}.
get_gateway(GatewayId) ->
	case mongodb_storage:find_one(?gatewayStorageName, [{'_id', GatewayId}]) of
		{ok, Plist} when is_list(Plist) ->
			{ok, proplist_to_record(Plist)};
		Error ->
			Error
	end.

-spec get_gateways() -> {ok, [{gateway_id(), #gateway{}}]} | {error, term()}.
get_gateways() ->
	case mongodb_storage:find(?gatewayStorageName, []) of
		{ok, List} ->
			{ok, [
				{Id, proplist_to_record(Plist)} || {Id, Plist} <- List
			]};
		Error ->
			Error
	end.

-spec del_gateway(gateway_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway(GatewayId) ->
	mongodb_storage:delete(?gatewayStorageName, [{'_id', GatewayId}]).

%% ===================================================================
%% Internals
%% ===================================================================

proplist_to_record(Plist) ->
	Name = proplists:get_value(name, Plist),
	RPS = proplists:get_value(rps, Plist),
	ConnectionsDoc = proplists:get_value(connections, Plist),
	ConnectionsPList = [bson:fields(Doc) || Doc <- ConnectionsDoc],
	Connections = [
		#connection{
			id = proplists:get_value(id, ConnPList),
			type = proplists:get_value(type, ConnPList),
			addr = proplists:get_value(addr, ConnPList),
			port = proplists:get_value(port, ConnPList),
			sys_id = proplists:get_value(sys_id, ConnPList),
			pass = proplists:get_value(pass, ConnPList),
			sys_type = proplists:get_value(sys_type, ConnPList),
			addr_ton = proplists:get_value(addr_ton, ConnPList),
			addr_npi = proplists:get_value(addr_npi, ConnPList),
			addr_range = proplists:get_value(addr_range, ConnPList)
		}
		|| ConnPList <- ConnectionsPList],
 	#gateway{
		name = Name,
		rps = RPS,
		connections = Connections
	}.
