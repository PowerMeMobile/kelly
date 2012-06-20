-module(k_http_api_handler_gateways).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	id :: list() | all
}).

%%% REST parameters
-record(get, {
}).

-record(create, {
		id = {mandatory, <<"id">>, list},
		name = {mandatory, <<"name">>, list},
		rps = {mandatory, <<"rps">>, integer}
}).

-record(update, {
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"gateway">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"gateways">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"gateway">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"gateway">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"gateway">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config_api:get_gateways() of
		{ok, GtwList} ->
			{ok, GtwsReady} = prepare_gtws(GtwList),
			?log_debug("GtwsReady: ~p", [GtwsReady]),
			{ok, {gateways, GtwsReady}, State};
		{error, Error} ->
			{ok, Error, State}
	end;

handle(_Req, #get{}, State = #state{id = Id}) ->
	case k_config_api:get_gateway(Id) of
		{ok, Gateway = #gateway{
			connections = Connections
			}} ->

			%% preparation connections' records
			ConnFun = ?record_to_proplist(connection),
			ConnPropList = lists:map(fun(ConnRec)->
				{_RecName, _PropList} = ConnFun(ConnRec)
			end, Connections),

			%% preparation gateway's record
			GatewayFun = ?record_to_proplist_(gateway),
			Response = GatewayFun(Gateway#gateway{
										connections = ConnPropList
										}, [{uuid, Id}]),
			?log_debug("Response: ~p", [Response]),
			erlang:halt(),

			{ok, Response, State};
		Any ->
			{ok, Any, State}
	end;

handle(_Req, #create{
	id = Id,
	name = Name,
	rps = RPS
}, State = #state{}) ->
	Gateway = #gateway{rps = RPS, name = Name},
	k_snmp:set_row(gtw, Id, [{gtwName, Name}, {gtwRPS, RPS}]),
	Res = k_config_api:set_gateway(Id, Gateway),
	{ok, {result, Res}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = Id}) ->
	k_snmp:del_row(gtw, Id),
	Res = k_config_api:del_gateway(Id),
	{ok, {result, Res}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

prepare_gtws(GtwList) when is_list(GtwList) ->
	prepare_gtws(GtwList, []).

prepare_gtws([], Acc) ->
	{ok, Acc};
prepare_gtws([{UUID, Gtw = #gateway{connections = Conns}} | Rest], Acc) ->
	%% preparation connections' records
	ConnFun = ?record_to_proplist(connection),
	ConnPropList = lists:map(fun(ConnRec)->
		{_RecName, _PropList} = ConnFun(ConnRec)
		end, Conns),
	%% preparation gateway's record
	GatewayFun = ?record_to_proplist_(gateway),
	GtwReady = GatewayFun(Gtw#gateway{
					connections = ConnPropList
				}, [{uuid, UUID}]),
	prepare_gtws(Rest, [GtwReady | Acc]).

