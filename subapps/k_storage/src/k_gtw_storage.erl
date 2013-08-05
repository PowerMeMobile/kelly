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
			'host'       , Conn#connection.host,
			'port'       , Conn#connection.port,
			'bind_type'  , bsondoc:atom_to_binary(Conn#connection.bind_type),
			'system_id'  , Conn#connection.system_id,
			'password'   , Conn#connection.password,
			'system_type', Conn#connection.system_type,
			'addr_ton'   , Conn#connection.addr_ton,
			'addr_npi'   , Conn#connection.addr_npi,
			'addr_range' , Conn#connection.addr_range
		} || Conn <- Gateway#gateway.connections
	],
	StsDoc = [
		{
			'name'        , Sts#setting.name,
			'value'       , Sts#setting.value
		} || Sts <- Gateway#gateway.settings
	],
	Modifier = {
		'$set', {
			'name'        , Gateway#gateway.name,
			'rps'         , Gateway#gateway.rps,
			'connections' , ConnectionsDocs,
			'settings'	  , StsDoc
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
			host = bsondoc:at(host, ConnDoc),
			port = bsondoc:at(port, ConnDoc),
			bind_type = bsondoc:binary_to_atom(bsondoc:at(bind_type, ConnDoc)),
			system_id = bsondoc:at(system_id, ConnDoc),
			password = bsondoc:at(password, ConnDoc),
			system_type = bsondoc:at(system_type, ConnDoc),
			addr_ton = bsondoc:at(addr_ton, ConnDoc),
			addr_npi = bsondoc:at(addr_npi, ConnDoc),
			addr_range = bsondoc:at(addr_range, ConnDoc)
		}
		|| ConnDoc <- ConnectionsDoc
	],
	StsDoc =
	case bsondoc:at(settings, Doc) of
		undefined -> [];
		Val -> Val
	end,
	Settings = [
		#setting{
			name = bsondoc:at(name, StDoc),
			value = bsondoc:at(value, StDoc)
		}
		|| StDoc <- StsDoc],
 	#gateway{
		name = Name,
		rps = RPS,
		connections = Connections,
		settings = Settings
	}.
