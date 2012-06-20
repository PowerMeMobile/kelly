-module(k_http_api_handler_networks).

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
	countryCode = {mandatory, <<"country_code">>, list},
	numbersLen = {mandatory, <<"numbers_len">>, integer},
	prefixes = {mandatory, <<"prefixes">>, list},
	providerId = {mandatory, <<"provider_id">>, list}
}).

-record(update, {
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"network">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"networks">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"network">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"network">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"network">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config_api:get_networks() of
		{ok, NtwList} ->
			{ok, GtwsReady} = prepare_ntws(NtwList),
			?log_debug("GtwsReady: ~p", [GtwsReady]),
			{ok, {networks, GtwsReady}, State};
		{error, Error} ->
			{ok, Error, State}
	end;

handle(_Req, #get{}, State = #state{id = NetworkId}) ->
	case k_config_api:get_network(NetworkId) of
		{ok, Network = #network{}} ->
			NetworkFun = ?record_to_proplist(network),
			Response = NetworkFun(Network),
			?log_debug("Response: ~p", [Response]),
			{ok, Response, State};
		Error ->
			{ok, Error, State}
	end;

handle(_Req, #create{
	id = Id,
	countryCode = CountryCode,
	numbersLen = NumbersLen,
	prefixes = Prefixes,
	providerId = ProviderId
}, State = #state{}) ->
	Network = #network{
			countryCode = CountryCode,
			numbersLen = NumbersLen,
			prefixes = split(Prefixes),
			providerId = ProviderId
	},
	Res = k_config_api:set_network(Id, Network),
	{ok, {result, Res}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = NetworkId}) ->
	Res = k_config_api:del_network(NetworkId),
	{ok, {result, Res}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

prepare_ntws(NtwList) ->
	prepare_ntws(NtwList, []).

prepare_ntws([], Acc) ->
	{ok, Acc};
prepare_ntws([{UUID, Ntw = #network{}} | Rest], Acc) ->
	NetworkFun = ?record_to_proplist_(network),
	NtwReady = NetworkFun(Ntw, [{uuid, UUID}]),
	?log_debug("NtwReady: ~p", [NtwReady]),
	prepare_ntws(Rest, [NtwReady | Acc]).

%% split "29,33,44" to ["29", "33", "44"]
split(Prefixes) ->
	string:tokens(Prefixes, ",").
