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
	ConnectionsDocs = [
		{
			'id'         , Conn#connection.id,
			'type'       , Conn#connection.type,
			'addr'       , Conn#connection.addr,
			'port'       , Conn#connection.port,
			'sys_id'     , Conn#connection.sys_id,
			'pass'       , Conn#connection.pass,
			'sys_type'   , Conn#connection.pass,
			'addr_ton'   , Conn#connection.addr_ton,
			'addr_npi'   , Conn#connection.addr_npi,
			'addr_range' , Conn#connection.addr_range
		} || Conn <- Gateway#gateway.connections
	],
	Modifier = {
		'$set', {
			'name'        , Gateway#gateway.name,
			'rps'         , Gateway#gateway.rps,
			'connections' , ConnectionsDocs
		}
	},
	mongodb_storage:upsert(k_static_storage, gateways, {'_id', GatewayId}, Modifier).

-spec get_gateway(gateway_id()) -> {ok, #gateway{}} | {error, no_entry} | {error, term()}.
get_gateway(GatewayId) ->
	case mongodb_storage:find_one(k_static_storage, gateways, {'_id', GatewayId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec get_gateways() -> {ok, [{gateway_id(), #gateway{}}]} | {error, term()}.
get_gateways() ->
	case mongodb_storage:find(k_static_storage, gateways, {}) of
		{ok, List} ->
			{ok, [
				{Id, doc_to_record(Doc)} || {Id, Doc} <- List
			]};
		Error ->
			Error
	end.

-spec del_gateway(gateway_id()) -> ok | {error, no_entry} | {error, term()}.
del_gateway(GatewayId) ->
	mongodb_storage:delete(k_static_storage, gateways, {'_id', GatewayId}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
	Name = bson:at(name, Doc),
	RPS = bson:at(rps, Doc),
	ConnectionsDoc = bson:at(connections, Doc),
	Connections = [
		#connection{
			id = bson:at(id, ConnDoc),
			type = bson:at(type, ConnDoc),
			addr = bson:at(addr, ConnDoc),
			port = bson:at(port, ConnDoc),
			sys_id = bson:at(sys_id, ConnDoc),
			pass = bson:at(pass, ConnDoc),
			sys_type = bson:at(sys_type, ConnDoc),
			addr_ton = bson:at(addr_ton, ConnDoc),
			addr_npi = bson:at(addr_npi, ConnDoc),
			addr_range = bson:at(addr_range, ConnDoc)
		}
		|| ConnDoc <- ConnectionsDoc],
 	#gateway{
		name = Name,
		rps = RPS,
		connections = Connections
	}.
