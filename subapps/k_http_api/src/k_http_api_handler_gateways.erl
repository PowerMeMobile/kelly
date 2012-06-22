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
			{ok, GtwPropLists} = prepare_gtws(GtwList),
			?log_debug("GtwPropLists: ~p", [GtwPropLists]),
			{ok, {gateways, GtwPropLists}, State};
		{error, Error} ->
			{ok, {error, io_lib:format("~p", [Error])}, State}
	end;

handle(_Req, #get{}, State = #state{id = GtwUUID}) ->
	case k_config_api:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{GtwUUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{ok, {gateway, GtwPropList}, State};
		{error, Error} ->
			{ok, {error, io_lib:format("~p", [Error])}, State}
	end;

handle(_Req, #create{
	id = Id,
	name = Name,
	rps = RPS
}, State = #state{}) ->
	Gateway = #gateway{rps = RPS, name = Name},
	k_snmp:set_row(gtw, Id, [{gtwName, Name}, {gtwRPS, RPS}]),
	Res = k_config_api:set_gateway(Id, Gateway),
	{ok, {result, io_lib:format("~p", [Res])}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = Id}) ->
	k_snmp:del_row(gtw, Id),
	Res = k_config_api:del_gateway(Id),
	{ok, {result, io_lib:format("~p", [Res])}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

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
	GtwPropList =  [{uuid, GtwUUID}] ++ GatewayFun(Gtw#gateway{
													connections = ConnPropLists
													}),
	prepare_gtws(Rest, [GtwPropList | Acc]).

