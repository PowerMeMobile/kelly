-module(k_http_api_handler_networks).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	id :: list() | all,
	network :: #network{}
}).

%%% REST parameters
-record(get, {
}).

-record(create, {
	id = {optional, <<"id">>, string_uuid},
	country_code = {mandatory, <<"country_code">>, integer},
	numbers_len = {mandatory, <<"numbers_len">>, integer},
	prefixes = {mandatory, <<"prefixes">>, list},
	provider_id = {mandatory, <<"provider_id">>, string_uuid}
}).

-record(update, {
	country_code = {optional, <<"country_code">>, integer},
	numbers_len = {optional, <<"numbers_len">>, integer},
	prefixes = {optional, <<"prefixes">>, list},
	provider_id = {optional, <<"provider_id">>, string_uuid}
}).

-record(delete, {
}).

init(_Req, 'GET', [_, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [_]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [_]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [_, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [_, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_config:get_networks() of
		{ok, NtwList} ->
			{ok, NtwPropLists} = prepare_ntws(NtwList),
			?log_debug("NtwPropLists: ~p", [NtwPropLists]),
			{http_code, 200, {networks, NtwPropLists}, State};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #get{}, State = #state{id = NtwUUID}) ->
	case k_config:get_network(NtwUUID) of
		{ok, Ntw = #network{}} ->
			{ok, [NtwPropList]} = prepare_ntws({NtwUUID, Ntw}),
			?log_debug("NtwPropList: ~p", [NtwPropList]),
			{http_code, 200, NtwPropList, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
	end;

handle(Req, Create = #create{id = undefined}, State = #state{}) ->
	UUID = k_uuid:to_string(k_uuid:newid()),
	create_network(Req, Create#create{id = UUID}, State);
handle(Req, Create = #create{id = ID}, State = #state{}) ->
	case k_config:get_network(ID) of
		{ok, #network{}} ->
			{exception, 'svc0004', [], State};
		{error, no_entry} ->
			create_network(Req, Create, State)
	end;

handle(Req, Update = #update{}, State = #state{id = ID}) ->
	case k_config:get_network(ID) of
		{ok, Network = #network{}} ->
			update_network(Req, Update, State#state{network = Network});
		{error, no_entry} ->
			{exception, 'svc0003', [], State}
	end;

handle(_Req, #delete{}, State = #state{id = NetworkId}) ->
	Res = k_config:del_network(NetworkId),
	{ok, {result, Res}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_network(_Req, Update, State = #state{id = ID, network = Network}) ->
 	NewCountryCode = resolve(Update#update.country_code, Network#network.countryCode),
	NewNumbersLen = resolve(Update#update.numbers_len, Network#network.numbersLen),
	NewPrefixes = resolve(split(Update#update.prefixes), Network#network.prefixes),
	NewProviderId = resolve(Update#update.provider_id, Network#network.providerId),
	Updated = #network{
		countryCode = NewCountryCode,
		numbersLen = NewNumbersLen,
		prefixes = NewPrefixes,
		providerId = NewProviderId
	},
	ok = k_config:set_network(ID, Updated),
	{ok, [NtwPropList]} = prepare_ntws({ID, Updated}),
	?log_debug("NtwPropList: ~p", [NtwPropList]),
	{http_code, 200, NtwPropList, State}.

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.

create_network(_Req, Create, State) ->
	#create{
		id = ID,
		country_code = CountryCode,
		numbers_len = NumbersLen,
		prefixes = Prefixes,
		provider_id = ProviderId} = Create,
 	Network = #network{
		countryCode = CountryCode,
		numbersLen = NumbersLen,
		prefixes = split(Prefixes),
		providerId = ProviderId
	},
	ok = k_config:set_network(ID, Network),
	{ok, [NtwPropList]} = prepare_ntws({ID, Network}),
	?log_debug("NtwPropList: ~p", [NtwPropList]),
	{http_code, 201, NtwPropList, State}.


prepare_ntws(NtwList) when is_list(NtwList) ->
	prepare_ntws(NtwList, []);
prepare_ntws(Ntw = {_UUID, #network{}}) ->
	prepare_ntws([Ntw]).

prepare_ntws([], Acc) ->
	{ok, Acc};
prepare_ntws([{NtwUUID, Ntw = #network{}} | Rest], Acc) ->
	NtwFun = ?record_to_proplist(network),
	NtwPropList = translate([{id, NtwUUID}] ++ NtwFun(Ntw)),
	prepare_ntws(Rest, [NtwPropList | Acc]).

%% split "29,33,44" to ["29", "33", "44"]
split(undefined) ->
	undefined;
split(Prefixes) ->
	string:tokens(Prefixes, ",").

translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(providerId) ->
	provider_id;
translate_name(numbersLen) ->
	numbers_len;
translate_name(countryCode) ->
	country_code;
translate_name(Name) ->
	Name.
