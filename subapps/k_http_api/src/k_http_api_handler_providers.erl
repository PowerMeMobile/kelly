-module(k_http_api_handler_providers).

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
	gateway = {mandatory, <<"gateway">>, list},
	bulkGateway = {mandatory, <<"bulk_gateway">>, list},
	receiptsSupported = {optional, <<"receipts_supported">>, boolean}
}).

-record(update, {
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"provider">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"providers">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"provider">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"provider">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"provider">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config_api:get_providers() of
		{ok, PrvList} ->
			{ok, PrvsReady} = prepare(PrvList),
			?log_debug("PrvsReady: ~p", [PrvsReady]),
			{ok, {providers, PrvsReady}, State};
		{error, Error} ->
			{ok, Error, State}
	end;


handle(_Req, #get{}, State = #state{id = ProviderId}) ->
	case k_config_api:get_provider(ProviderId) of
		{ok, Provider = #provider{}} ->
			ProviderFun = ?record_to_proplist(provider),
			Response = ProviderFun(Provider),
			?log_debug("Response: ~p", [Response]),
			{ok, Response, State};
		ErrorResponse ->
			{ok, ErrorResponse, State}
	end;

handle(_Req, #create{
	id = Id,
	gateway = Gateway,
	bulkGateway = BulkGateway,
	receiptsSupported = ReceiptsSupported
}, State = #state{}) ->
	Provider = #provider{
		gateway = Gateway,
		bulkGateway = BulkGateway,
		receiptsSupported = ReceiptsSupported
	},
	Res = k_config_api:set_provider(Id, Provider),
	{ok, {result, Res}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = ProviderId}) ->
	Res = k_config_api:del_provider(ProviderId),
	{ok, {result, Res}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

prepare(PrvList) when is_list(PrvList) ->
	prepare(PrvList, []).

prepare([], Acc) ->
	{ok, Acc};
prepare([{UUID, Prv = #provider{}} | Rest], Acc) ->
	ProviderFun = ?record_to_proplist_(provider),
	ReadyPrv = ProviderFun(Prv, [{uuid, UUID}]),
	prepare(Rest, [ReadyPrv | Acc]).


