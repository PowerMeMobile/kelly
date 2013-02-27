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
	Name = bsondoc:at(name, Doc),
	RPS = bsondoc:at(rps, Doc),
	ConnectionsDoc = bsondoc:at(connections, Doc),
	Connections = [
		#connection{
			id = bsondoc:at(id, ConnDoc),
			type = bsondoc:at(type, ConnDoc),
			addr = bsondoc:at(addr, ConnDoc),
			port = bsondoc:at(port, ConnDoc),
			sys_id = bsondoc:at(sys_id, ConnDoc),
			pass = bsondoc:at(pass, ConnDoc),
			sys_type = bsondoc:at(sys_type, ConnDoc),
			addr_ton = bsondoc:at(addr_ton, ConnDoc),
			addr_npi = bsondoc:at(addr_npi, ConnDoc),
			addr_range = bsondoc:at(addr_range, ConnDoc)
		}
		|| ConnDoc <- ConnectionsDoc],
 	#gateway{
		name = Name,
		rps = RPS,
		connections = Connections
	}.
