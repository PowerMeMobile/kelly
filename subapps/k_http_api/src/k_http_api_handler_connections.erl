-module(k_http_api_handler_connections).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	gtwid :: string(),
	cnnid :: integer(),
	gtw :: #gateway{},
	conn :: #connection{}
}).

%%% REST parameters

-record(get, {}).

-record(create, {
	id 			= {optional, <<"id">>, integer},
	type 		= {mandatory, <<"type">>, integer},
	addr 		= {mandatory, <<"addr">>, list},
	port 		= {mandatory, <<"port">>, integer},
	sys_id 		= {mandatory, <<"sys_id">>, list},
	pass 		= {mandatory, <<"pass">>, list},
	sys_type 	= {mandatory, <<"sys_type">>, list},
	addr_ton 	= {mandatory, <<"addr_ton">>, integer},
	addr_npi 	= {mandatory, <<"addr_npi">>, integer},
	addr_range	= {mandatory, <<"addr_range">>, list}
}).

-record(update, {
	type 		= {optional, <<"type">>, integer},
	addr 		= {optional, <<"addr">>, list},
	port 		= {optional, <<"port">>, integer},
	sys_id 		= {optional, <<"sys_id">>, list},
	pass 		= {optional, <<"pass">>, list},
	sys_type 	= {optional, <<"sys_type">>, list},
	addr_ton 	= {optional, <<"addr_ton">>, integer},
	addr_npi 	= {optional, <<"addr_npi">>, integer},
	addr_range	= {optional, <<"addr_range">>, list}
}).

-record(delete, {
}).

init(_Req, 'GET', [_, GtwIDBin, _]) ->
	GtwID = binary_to_list(GtwIDBin),
	{ok, #get{}, #state{gtwid = GtwID, cnnid = all}};

init(_Req, 'GET', [_, GtwIDBin, _, ConnIDBin]) ->
	GtwID = binary_to_list(GtwIDBin),
	ConnID = list_to_integer(binary_to_list(ConnIDBin)),
	{ok, #get{}, #state{gtwid = GtwID, cnnid = ConnID}};

init(_Req, 'POST', [_, GtwIDBin, _]) ->
	GtwID = binary_to_list(GtwIDBin),
	{ok, #create{}, #state{gtwid = GtwID}};

init(_Req, 'PUT', [_, GtwIDBin, _, ConnIDBin]) ->
	GtwID = binary_to_list(GtwIDBin),
	ConnID = list_to_integer(binary_to_list(ConnIDBin)),
	{ok, #update{}, #state{gtwid = GtwID, cnnid = ConnID}};

init(_Req, 'DELETE', [_, GtwIDBin, _, ConnIDBin]) ->
	GtwID = binary_to_list(GtwIDBin),
	ConnID = list_to_integer(binary_to_list(ConnIDBin)),
	{ok, #delete{}, #state{gtwid = GtwID, cnnid = ConnID}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{gtwid = GtwId, cnnid = all}) ->
	case k_config:get_gateway(GtwId) of
		{ok, #gateway{connections = Connections}} ->
			{ok, ConnsPropList} = prepare_connections(Connections),
			?log_debug("ConnsPropList: ~p", [ConnsPropList]),
			{http_code, 200, {connections, ConnsPropList}, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
   	end;

handle(_Req, #get{}, State = #state{gtwid = GtwID, cnnid = ConnID}) ->
	case k_config:get_gateway(GtwID) of
		{ok, #gateway{connections = Connections}} ->
			case get_connection(ConnID, Connections) of
				undefined ->
					?log_debug("Connection [~p] not found", [ConnID]),
					{exception, 'svc0003', [], State};
				Connection = #connection{} ->
					{ok, [ConnPropList]} = prepare_connections(Connection),
					?log_debug("ConnPropList: ~p", [ConnPropList]),
					{http_code, 200, ConnPropList, State}
			end;
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
   	end;

handle(Req, Create = #create{}, State = #state{gtwid = GtwId}) ->
	case k_config:get_gateway(GtwId) of
		{ok, Gtw = #gateway{}} ->
			get_connection(Req, Create, State#state{gtw = Gtw});
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
   	end;

handle(Req, Update = #update{}, State = #state{gtwid = GtwID}) ->
	case k_config:get_gateway(GtwID) of
		{ok, Gtw = #gateway{}} ->
			update_connection(Req, Update, State#state{gtw = Gtw});
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
   	end;

handle(_Req, #delete{}, State = #state{gtwid = GtwId, cnnid = ConnId}) ->
	ok = k_config:del_gateway_connection(GtwId, ConnId),
	k_snmp:del_row(cnn, GtwId ++ [ConnId]),
	{http_code, 204, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_connection(_Req, Update, State = #state{cnnid = ConnectionID, gtw = Gtw, gtwid = GtwID}) ->
	#gateway{connections = Connections} = Gtw,
	case get_connection(ConnectionID, Connections) of
		undefined -> {exception, 'svc0003', [], State};
		Conn = #connection{} ->
			NewType = resolve(Update#update.type, Conn#connection.type),
			NewAddr = resolve(Update#update.addr, Conn#connection.addr),
			NewPort = resolve(Update#update.port, Conn#connection.port),
			NewSysId = resolve(Update#update.sys_id, Conn#connection.sys_id),
			NewPass = resolve(Update#update.pass, Conn#connection.pass),
			NewSysType = resolve(Update#update.sys_type, Conn#connection.sys_type),
			NewAddrTon = resolve(Update#update.addr_ton, Conn#connection.addr_ton),
			NewAddrNpi = resolve(Update#update.addr_npi, Conn#connection.addr_npi),
			NewAddrRange = resolve(Update#update.addr_range, Conn#connection.addr_range),
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
			{http_code, 200, ConnPropList, State}
	end.

replace_connection(NewConnection, Connections) ->
	replace_connection(NewConnection, Connections, []).
replace_connection(_, [], Connections) ->
	{ok, Connections};
replace_connection(NewConn = #connection{id = ID}, [#connection{id = ID} | Tail], Acc) ->
	replace_connection(NewConn, Tail, [NewConn | Acc]);
replace_connection(NewConn, [Conn | Tail], Acc) ->
	replace_connection(NewConn, Tail, [Conn | Acc]).

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.

get_next_connection_id(Connections) ->
	get_next_connection_id(Connections, 0).
get_next_connection_id([], MaxID) ->
	{ok, MaxID + 1};
get_next_connection_id([#connection{id = ID} | Tail], MaxID) when ID > MaxID ->
	get_next_connection_id(Tail, ID);
get_next_connection_id([_ | Tail], MaxID) ->
	get_next_connection_id(Tail, MaxID).

get_connection(Req, Create = #create{id = undefined}, State = #state{gtw = Gtw}) ->
	#gateway{connections = Connections} = Gtw,
	{ok, NextID} = get_next_connection_id(Connections),
	?log_debug("Next connection id: ~p", [NextID]),
	create_connection(Req, Create#create{id = NextID}, State);
get_connection(Req, Create = #create{id = Id}, State = #state{gtw = Gtw}) ->
	#gateway{connections = Conns} = Gtw,
	case get_connection(Id, Conns) of
		Conn = #connection{} ->
			?log_debug("Connection [~p] already exist", [Id]),
			{exception, 'svc0004', [], State#state{conn = Conn}};
		undefined -> create_connection(Req, Create, State)
	end.

get_connection(_ID, []) ->
	undefined;
get_connection(ID, [Conn = #connection{id = ID} | _Tail]) ->
	Conn;
get_connection(ID, [_Conn | Tail]) ->
	get_connection(ID, Tail).

create_connection(_Req, #create{
						id 			= CnnId,
						type 		= Type,
						addr 		= Addr,
						port 		= Port,
						sys_id 		= SysId,
						pass 		= Pass,
						sys_type 	= SysType,
						addr_ton 	= AddrTon,
						addr_npi 	= AddrNpi,
						addr_range	= AddrRange
					}, State = #state{gtwid = GtwId}) ->

	Connection = #connection{
		id 			= CnnId,
		type 		= Type,
		addr 		= Addr,
		port 		= Port,
		sys_id 		= SysId,
		pass 		= Pass,
		sys_type 	= SysType,
		addr_ton 	= AddrTon,
		addr_npi 	= AddrNpi,
		addr_range	= AddrRange
	},
	k_config:set_gateway_connection(GtwId, Connection),

	SnmpConnId = GtwId ++ [CnnId],
	k_snmp:set_row(cnn, SnmpConnId,
		[{cnnType, Type},
		{cnnAddr, convert_to_snmp_ip(Addr)},
		{cnnPort, Port},
		{cnnSystemId, SysId},
		{cnnPassword, Pass},
		{cnnSystemType, SysType},
		{cnnAddrTon, AddrTon},
		{cnnAddrNpi, AddrNpi},
		{cnnAddrRange, AddrRange}
	    ]),
	{ok, [ConnPropList]} = prepare_connections(Connection),
	?log_debug("ConnPropList: ~p", [ConnPropList]),
	{http_code, 201, ConnPropList, State}.

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
	IP = lists:map(fun(Token)->
		list_to_integer(Token)
	end, Tokens),
	IP.
