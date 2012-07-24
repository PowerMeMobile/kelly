-module(k_http_api_handler_providers).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	id :: list() | all,
	provider :: #provider{}
}).

%%% REST parameters

-record(get, {
}).

-record(create, {
	id = {optional, <<"id">>, string_uuid},
	gateway = {mandatory, <<"gateway">>, string_uuid},
	bulk_gateway = {mandatory, <<"bulk_gateway">>, string_uuid},
	receipts_supported = {mandatory, <<"receipts_supported">>, boolean}
}).

-record(update, {
	gateway = {optional, <<"gateway">>, string_uuid},
	bulk_gateway = {optional, <<"bulk_gateway">>, string_uuid},
	receipts_supported = {optional, <<"receipts_supported">>, boolean}
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"providers">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"providers">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"providers">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"providers">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"providers">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config:get_providers() of
		{ok, PrvList} ->
			{ok, PrvPropLists} = prepare(PrvList),
			?log_debug("PrvPropLists: ~p", [PrvPropLists]),
			{http_code, 200, {providers, PrvPropLists}, State};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #get{}, State = #state{id = PrvUUID}) ->
	case k_config:get_provider(PrvUUID) of
		{ok, Prv = #provider{}} ->
			{ok, [PrvPropList]} = prepare({PrvUUID, Prv}),
			?log_debug("PrvPropList: ~p", [PrvPropList]),
			{http_code, 200, PrvPropList, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(Req, Create = #create{id = undefined}, State) ->
	StringUUID = k_uuid:to_string(k_uuid:newid()),
	create_provider(Req, Create#create{id = StringUUID}, State);

handle(Req, Create = #create{id = ID}, State = #state{}) ->
	case k_config:get_provider(ID) of
		{ok, #provider{}} ->
			{exception, 'svc0004', [], State};
		{error, no_entry} ->
			create_provider(Req, Create, State);
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(Req, Update = #update{}, State = #state{id = ID}) ->
	case k_config:get_provider(ID) of
		{ok, Provider = #provider{}} ->
			update_provider(Req, Update, State#state{provider = Provider});
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #delete{}, State = #state{id = ProviderId}) ->
	ok = k_config:del_provider(ProviderId),
	{http_code, 204, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_provider(_Req, Update, State = #state{id = ID, provider = Provider}) ->
	Gateway = resolve(Update#update.gateway, Provider#provider.gateway),
	BulkGateway = resolve(Update#update.bulk_gateway, Provider#provider.bulkGateway),
	ReceiptsSupported = resolve(Update#update.receipts_supported, Provider#provider.receiptsSupported),
	Updated = #provider{
		gateway = Gateway,
		bulkGateway = BulkGateway,
		receiptsSupported = ReceiptsSupported},
	ok = k_config:set_provider(ID, Updated),
	{ok, [PrvPropList]} = prepare({ID, Updated}),
	?log_debug("PrvPropList: ~p", [PrvPropList]),
	{http_code, 200, PrvPropList, State}.

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.

create_provider(_Req, Create = #create{}, State) ->
	#create{
		id = ID,
		gateway = Gateway,
		bulk_gateway = BulkGateway,
		receipts_supported = ReceiptsSupported} = Create,
	Provider = #provider{
		gateway = Gateway,
		bulkGateway = BulkGateway,
		receiptsSupported = ReceiptsSupported
	},
	ok = k_config:set_provider(ID, Provider),
	{ok, [PrvPropList]} = prepare({ID, Provider}),
	?log_debug("PrvPropList: ~p", [PrvPropList]),
	{http_code, 201, PrvPropList, State}.

prepare(PrvList) when is_list(PrvList) ->
	prepare(PrvList, []);
prepare(Prv = {_UUID, #provider{}}) ->
	prepare([Prv]).

prepare([], Acc) ->
	{ok, Acc};
prepare([{PrvUUID, Prv = #provider{}} | Rest], Acc) ->
	PrvFun = ?record_to_proplist(provider),
	PrvPropList = [{id, PrvUUID}] ++ PrvFun(Prv),
	prepare(Rest, [PrvPropList | Acc]).


