-module(k_http_api_handler_connections).

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
%% Callback Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"gateways">>, gateway_id, <<"connections">>, id],
				params = [#param{name = gateway_id, mandatory = true, repeated = false, type = string_uuid},
						  #param{name = id, mandatory = true, repeated = false, type = integer}]},
			#method_spec{
				path = [<<"gateways">>, gateway_id, <<"connections">>],
				params = [#param{name = gateway_id, mandatory = true, repeated = false, type = string_uuid}]}],

	UpdateParams = [
		#param{name = gateway_id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = id, mandatory = true, repeated = false, type = integer},
		#param{name = type, mandatory = false, repeated = false, type = integer},
		#param{name = addr,	mandatory = false, repeated = false,	type = string},
		#param{name = port, mandatory = false, repeated = false, type = integer},
		#param{name = sys_id, mandatory = false, repeated = false, type = string},
		#param{name = pass, mandatory = false, repeated = false, type = string},
		#param{name = sys_type, mandatory = false, repeated = false, type = string},
		#param{name = addr_ton, mandatory = false, repeated = false, type = integer},
		#param{name = addr_npi, mandatory = false, repeated = false, type = integer},
		#param{name = addr_range, mandatory = false, repeated = false, type = string}
	],
	Update = #method_spec{
				path = [<<"gateways">>, gateway_id, <<"connections">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = gateway_id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = id, mandatory = true, repeated = false, type = integer}
	],
	Delete = #method_spec{
				path = [<<"gateways">>, gateway_id, <<"connections">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = gateway_id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = id, mandatory = false, repeated = false, type = integer},
		#param{name = type, mandatory = true, repeated = false, type = integer},
		#param{name = addr,	mandatory = true, repeated = false,	type = string},
		#param{name = port, mandatory = true, repeated = false, type = integer},
		#param{name = sys_id, mandatory = true, repeated = false, type = string},
		#param{name = pass, mandatory = true, repeated = false, type = string},
		#param{name = sys_type, mandatory = true, repeated = false, type = string},
		#param{name = addr_ton, mandatory = true, repeated = false, type = integer},
		#param{name = addr_npi, mandatory = true, repeated = false, type = integer},
		#param{name = addr_range, mandatory = true, repeated = false, type = string}
	],
	Create = #method_spec{
				path = [<<"gateways">>, gateway_id, <<"connections">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = Update,
			delete = Delete
		}}.

read(Params) ->
	ConnectionID = ?gv(id, Params),
	case ConnectionID of
		undefined -> read_all(?gv(gateway_id, Params));
		_ -> read_id(?gv(gateway_id, Params), ConnectionID)
	end.

create(Params) ->
	GtwUUID = ?gv(gateway_id, Params),
	case k_config:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			check_connection_id(Gtw, Params);
		{error, no_entry} ->
			{exception, 'svc0003'}
   	end.

update(Params) ->
	GtwUUID = ?gv(gateway_id, Params),
	case k_config:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			update_connection(Gtw, Params);
		{error, no_entry} ->
			{exception, 'svc0003'}
   	end.

delete(Params) ->
	GtwUUID = ?gv(gateway_id, Params),
	ConnectionID = ?gv(id, Params),
	ok = k_config:del_gateway_connection(GtwUUID, ConnectionID),
	k_snmp:del_row(cnn, GtwUUID ++ [ConnectionID]),
	{http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all(GatewayID) ->
	case k_config:get_gateway(GatewayID) of
		{ok, #gateway{connections = Connections}} ->
			{ok, ConnsPropList} = prepare_connections(Connections),
			?log_debug("ConnsPropList: ~p", [ConnsPropList]),
			{http_code, 200, {connections, ConnsPropList}};
		{error, no_entry} ->
			{exception, 'svc0003'}
   	end.

read_id(GatewayID, ConnectionID) ->
	case k_config:get_gateway(GatewayID) of
		{ok, #gateway{connections = Connections}} ->
			case get_connection(ConnectionID, Connections) of
				undefined ->
					?log_debug("Connection [~p] not found", [ConnectionID]),
					{exception, 'svc0003'};
				Connection = #connection{} ->
					{ok, [ConnPropList]} = prepare_connections(Connection),
					?log_debug("ConnPropList: ~p", [ConnPropList]),
					{http_code, 200, ConnPropList}
			end;
		{error, no_entry} ->
			{exception, 'svc0003'}
   	end.

update_connection(Gtw, Params) ->
	ConnectionID = ?gv(id, Params),
	GtwID = ?gv(gateway_id, Params),
	#gateway{connections = Connections} = Gtw,
	case get_connection(ConnectionID, Connections) of
		undefined -> {exception, 'svc0003'};
		Conn = #connection{} ->
			NewType = resolve(type, Params, Conn#connection.type),
			NewAddr = resolve(addr, Params, Conn#connection.addr),
			NewPort = resolve(port, Params, Conn#connection.port),
			NewSysId = resolve(sys_id, Params, Conn#connection.sys_id),
			NewPass = resolve(pass, Params, Conn#connection.pass),
			NewSysType = resolve(sys_type, Params, Conn#connection.sys_type),
			NewAddrTon = resolve(addr_ton, Params, Conn#connection.addr_ton),
			NewAddrNpi = resolve(addr_npi, Params, Conn#connection.addr_npi),
			NewAddrRange = resolve(addr_range, Params, Conn#connection.addr_range),
			NewConnection =
				#connection{
					id = ConnectionID,
					type = NewType,
					addr = NewAddr,
					port = NewPort,
					sys_id = NewSysType,
					pass = NewPass,
					sys_type = NewSysType,
					addr_ton = NewAddrTon,
					addr_npi = NewAddrNpi,
					addr_range = NewAddrRange
					},
 			{ok, NewConnections} = replace_connection(NewConnection, Connections),
			ok = k_config:set_gateway(GtwID, Gtw#gateway{connections = NewConnections}),
			SnmpConnID = GtwID ++ [ConnectionID],
			k_snmp:set_row(cnn, SnmpConnID,
				[{cnnType, NewType},
				{cnnAddr, convert_to_snmp_ip(NewAddr)},
				{cnnPort, NewPort},
				{cnnSystemId, NewSysId},
				{cnnPassword, NewPass},
				{cnnSystemType, NewSysType},
				{cnnAddrTon, NewAddrTon},
				{cnnAddrNpi, NewAddrNpi},
				{cnnAddrRange, NewAddrRange}
			    ]),
			{ok, [ConnPropList]} = prepare_connections(NewConnection),
			?log_debug("ConnPropList: ~p", [ConnPropList]),
			{http_code, 200, ConnPropList}
	end.

replace_connection(NewConnection, Connections) ->
	replace_connection(NewConnection, Connections, []).
replace_connection(_, [], Connections) ->
	{ok, Connections};
replace_connection(NewConn = #connection{id = ID}, [#connection{id = ID} | Tail], Acc) ->
	replace_connection(NewConn, Tail, [NewConn | Acc]);
replace_connection(NewConn, [Conn | Tail], Acc) ->
	replace_connection(NewConn, Tail, [Conn | Acc]).


get_next_connection_id(Connections) ->
	get_next_connection_id(Connections, 0).
get_next_connection_id([], MaxID) ->
	{ok, MaxID + 1};
get_next_connection_id([#connection{id = ID} | Tail], MaxID) when ID > MaxID ->
	get_next_connection_id(Tail, ID);
get_next_connection_id([_ | Tail], MaxID) ->
	get_next_connection_id(Tail, MaxID).

check_connection_id(Gtw = #gateway{connections = Connections}, Params) ->
	case ?gv(id, Params) of
		undefined ->
			{ok, NextID} = get_next_connection_id(Connections),
			?log_debug("Next connection id: ~p", [NextID]),
			create_connection(lists:keyreplace(id, 1, Params, {id, NextID}));
		_ ->
			is_connections_exist(Gtw, Params)
	end.

is_connections_exist(#gateway{connections = Connections}, Params) ->
	ConnectionID = ?gv(id, Params),
	case get_connection(ConnectionID, Connections) of
		#connection{} ->
			?log_debug("Connection [~p] already exist", [ConnectionID]),
			{exception, 'svc0004'};
		undefined ->
			create_connection(Params)
	end.


get_connection(_ID, []) ->
	undefined;
get_connection(ID, [Conn = #connection{id = ID} | _Tail]) ->
	Conn;
get_connection(ID, [_Conn | Tail]) ->
	get_connection(ID, Tail).

create_connection(Params) ->
	ConnectionID = ?gv(id, Params),
	Type = ?gv(type, Params),
	Addr = ?gv(addr, Params),
	Port = ?gv(port, Params),
	SysID = ?gv(sys_id, Params),
	Pass = ?gv(pass, Params),
	SysType = ?gv(sys_type, Params),
	AddrTON = ?gv(addr_ton, Params),
	AddrNPI= ?gv(addr_npi, Params),
	AddrRange = ?gv(addr_range, Params),
	Connection = #connection{
		id 			= ConnectionID,
		type 		= Type,
		addr 		= Addr,
		port 		= Port,
		sys_id 		= SysID,
		pass 		= Pass,
		sys_type 	= SysType,
		addr_ton 	= AddrTON,
		addr_npi 	= AddrNPI,
		addr_range	= AddrRange
	},
	GtwUUID = ?gv(gateway_id, Params),
	k_config:set_gateway_connection(GtwUUID, Connection),

	SnmpConnId = GtwUUID ++ [ConnectionID],
	k_snmp:set_row(cnn, SnmpConnId,
		[{cnnType, Type},
		{cnnAddr, convert_to_snmp_ip(Addr)},
		{cnnPort, Port},
		{cnnSystemId, SysID},
		{cnnPassword, Pass},
		{cnnSystemType, SysType},
		{cnnAddrTon, AddrTON},
		{cnnAddrNpi, AddrNPI},
		{cnnAddrRange, AddrRange}
	    ]),
	{ok, [ConnPropList]} = prepare_connections(Connection),
	?log_debug("ConnPropList: ~p", [ConnPropList]),
	{http_code, 201, ConnPropList}.

prepare_connections(ConnectionsList) when is_list(ConnectionsList) ->
	prepare_connections(ConnectionsList, []);
prepare_connections(Connection = #connection{}) ->
	prepare_connections([Connection], []).

prepare_connections([], Acc) ->
	{ok, Acc};
prepare_connections([Connection = #connection{} | Rest], Acc) ->

	%% convert connections records to proplists
	ConnFun = ?record_to_proplist(connection),
	ConnPropList = ConnFun(Connection),
	prepare_connections(Rest, [ConnPropList | Acc]).


%% convert "127.0.0.1" to [127,0,0,1]
convert_to_snmp_ip(Addr) when is_list(Addr) ->
	Tokens = string:tokens(Addr, "."),
	lists:map(fun(Token)->
		list_to_integer(Token)
	end, Tokens).
