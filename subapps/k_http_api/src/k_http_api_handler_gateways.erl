
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
		id = {optional, <<"id">>, string_uuid},
		name = {mandatory, <<"name">>, list},
		rps = {mandatory, <<"rps">>, integer}
}).

-record(update, {
		name = {optional, <<"name">>, list},
		rps = {optional, <<"rps">>, integer}
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"gateways">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"gateways">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"gateways">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"gateways">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"gateways">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_warn("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config:get_gateways() of
		{ok, GtwList} ->
			{ok, GtwPropLists} = prepare_gtws(GtwList),
			?log_debug("GtwPropLists: ~p", [GtwPropLists]),
			{ok, {gateways, GtwPropLists}, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
   	end;

handle(_Req, #get{}, State = #state{id = GtwUUID}) ->
	case k_config:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{GtwUUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{ok, GtwPropList, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
	end;

handle(Req, Params = #create{id = undefined}, State = #state{}) ->
	UUID = k_uuid:to_string(k_uuid:newid()),
	create_gtw(Req, Params#create{id=UUID}, State);
handle(Req, Params = #create{id = UUID}, State = #state{}) ->
	case k_config:get_gateway(UUID) of
		{ok, #gateway{}} ->
			?log_warn("Gateway already exist. Abort.", []),
			{exception, 'svc0004', [], State};
		{error, no_entry} ->
			create_gtw(Req, Params, State)
	end;

handle(Req, Update = #update{}, State = #state{id = GtwUUID}) ->
	case k_config:get_gateway(GtwUUID) of
		{ok, Gtw = #gateway{}} ->
			update_gtw(Req, Update, Gtw, State);
			%% {ok, [GtwPropList]} = prepare_gtws([{GtwUUID, Gtw}]),
			%% ?log_debug("GtwPropList: ~p", [GtwPropList]),
			%% {ok, GtwPropList, State};
		{error, no_entry} ->
			{exception, 'svc0004', [], State}
	end;

handle(_Req, #delete{}, State = #state{id = Id}) ->
	k_snmp:del_row(gtw, Id),
	ok = k_config:del_gateway(Id),
	{http_code, 204, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_gtw(_Req, Update, Gtw, State = #state{id = UUID}) ->
	#update{
		rps = ReqRPS,
		name = ReqName} = Update,
	?log_debug("Update: ~p", [Update]),
	#gateway{rps = RPS, name = Name, connections = Conns} = Gtw,
	NewRPS = resolve_parameter(ReqRPS, RPS),
	NewName = resolve_parameter(ReqName, Name),
	NewGtw = #gateway{rps = NewRPS, name = NewName, connections = Conns},
	?log_debug("New gtw: ~p", [NewGtw]),
	%% Gateway = #gateway{rps = RPS, name = Name},
	k_snmp:set_row(gtw, UUID, [{gtwName, NewName}, {gtwRPS, NewRPS}]),
	ok = k_config:set_gateway(UUID, NewGtw),
	case k_config:get_gateway(UUID) of
		{ok, NewGtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{UUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{http_code, 201, GtwPropList, State};
		{error, no_entry} ->
			?log_warn("Gateway not found after creation [~p]", [UUID]),
			{http_code, 500, State};
		Any ->
			?log_error("Unexpected error: ~p", [Any]),
			{http_code, 500, State}
	end.

resolve_parameter(undefined, ExistingValue) ->
	ExistingValue;
resolve_parameter(NewValue, _ExistingValue) ->
	NewValue.

create_gtw(_Req, Create, State) ->
	#create{
		id = UUID,
		rps = RPS,
		name = Name} = Create,
	Gateway = #gateway{rps = RPS, name = Name},
	k_snmp:set_row(gtw, UUID, [{gtwName, Name}, {gtwRPS, RPS}]),
	ok = k_config:set_gateway(UUID, Gateway),
	case k_config:get_gateway(UUID) of
		{ok, Gtw = #gateway{}} ->
			{ok, [GtwPropList]} = prepare_gtws([{UUID, Gtw}]),
			?log_debug("GtwPropList: ~p", [GtwPropList]),
			{http_code, 201, GtwPropList, State};
		{error, no_entry} ->
			?log_warn("Gateway not found after creation [~p]", [UUID]),
			{http_code, 500, State};
		Any ->
			?log_error("Unexpected error: ~p", [Any]),
			{http_code, 500, State}
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
	GtwPropList =  [{id, GtwUUID}] ++ GatewayFun(Gtw#gateway{
													connections = ConnPropLists
													}),
	prepare_gtws(Rest, [GtwPropList | Acc]).

