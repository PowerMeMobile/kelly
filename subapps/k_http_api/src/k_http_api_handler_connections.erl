-module(k_http_api_handler_connections).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	gtwid :: string(),
	cnnid :: integer()
}).

%%% REST parameters

-record(create, {
	id 			= {mandatory, <<"id">>, integer},
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
}).

-record(delete, {
}).

init(_Req, 'POST', [<<"gateway">>, GtwIdBin, <<"connection">>]) ->
	GtwId = binary_to_list(GtwIdBin),
	{ok, #create{}, #state{gtwid = GtwId}};

init(_Req, 'PUT', [<<"gateway">>, _GtwIdBin, <<"connection">>, _ConnId]) ->
	{ok, #update{}, #state{}};

init(_Req, 'DELETE', [<<"gateway">>, GtwIdBin, <<"connection">>, ConnIdBin]) ->
	GtwId = binary_to_list(GtwIdBin),
	ConnId = list_to_integer(binary_to_list(ConnIdBin)),
	{ok, #delete{}, #state{gtwid = GtwId, cnnid = ConnId}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.


handle(_Req, #create{
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
	k_config_api:set_gateway_connection(GtwId, Connection),

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

	{ok, {result, ok}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, "update not implemented"}, State};

handle(_Req, #delete{}, State = #state{gtwid = GtwId, cnnid = ConnId}) ->
	k_config_api:del_gateway_connection(GtwId, ConnId),
	k_snmp:del_row(cnn, GtwId ++ [ConnId]),
	{ok, {result, deleted}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%%% Local functions

%% convert "127.0.0.1" to [127,0,0,1]
convert_to_snmp_ip(Addr) when is_list(Addr) ->
	Tokens = string:tokens(Addr, "."),
	IP = lists:map(fun(Token)->
		list_to_integer(Token)
	end, Tokens),
	IP.
