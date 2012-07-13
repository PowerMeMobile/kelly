-module(k_http_api_handler_customers).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").

-record(state, {
	id :: list() | all
}).

%%% REST parameters
-record(get, {
}).

-record(create, {
	id = {mandatory, <<"id">>, list},
	uuid = {mandatory, <<"uuid">>, list},
	name = {mandatory, <<"name">>, list},
	priority = {mandatory, <<"priority">>, integer},
	rps = {optional, <<"rps">>, integer},
	allowedSources = {mandatory, <<"allowed_sources">>, list},
	defaultSource = {optional, <<"default_source">>, list},
	networks = {mandatory, <<"networks">>, list},
	defaultProviderId = {optional, <<"default_provider_id">>, list},
	receiptsAllowed = {optional, <<"receipts_allowed">>, boolean},
	noRetry = {optional, <<"no_retry">>, boolean},
	defaultValidity = {mandatory, <<"default_validity">>, list},
	maxValidity = {mandatory, <<"max_validity">>, integer},
	state = {mandatory, <<"state">>, integer}
}).

-record(update, {
}).

-record(delete, {
}).

init(_Req, 'GET', [<<"customer">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #get{}, #state{id = Id}};

init(_Req, 'GET', [<<"customers">>]) ->
	{ok, #get{}, #state{id = all}};

init(_Req, 'POST', [<<"customer">>]) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', [<<"customer">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #update{}, #state{id = Id}};

init(_Req, 'DELETE', [<<"customer">>, BinId]) ->
	Id = binary_to_list(BinId),
	{ok, #delete{}, #state{id = Id}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{id = all}) ->
	case k_aaa:get_customers() of
 		{ok, CustList} ->
			{ok, CustPropLists} = prepare(CustList),
			?log_debug("CustPropLists: ~p", [CustPropLists]),
			{ok, {customers, CustPropLists}, State};
		{error, Error} ->
			{ok, {error, io_lib:format("~p", [Error])}, State}
	end;

handle(_Req, #get{}, State = #state{id = CustSysID}) ->
	case k_aaa:get_customer_by_system_id(CustSysID) of
		{ok, Customer = #customer{uuid = UUID}} ->
			{ok, [CustPropList]} = prepare({UUID, Customer}),
			?log_debug("CustPropList: ~p", [CustPropList]),
			{ok, {customer, CustPropList}, State};
		Any ->
			{ok, {error, io_lib:format("~p", [Any])}, State}
	end;

handle(_Req, #create{
			id = Id,
			uuid = UUID,
			name = Name,
			priority = Priority,
			rps = Rps,
			allowedSources = AddrString,
			defaultSource = DefaultSource,
			networks = NetworksString,
			defaultProviderId = DefaultProviderId,
			receiptsAllowed = ReceiptsAllowed,
			noRetry = NoRetry,
			defaultValidity = DefaultValidity,
			maxValidity = MaxValidity,
			state = CustState
					}, State = #state{}) ->

	Customer = #customer{
			id = Id,
			uuid = UUID,
			name = Name,
			priority = Priority,
			rps = Rps,
			allowedSources = decode_addr(AddrString),
			defaultSource = DefaultSource,
			networks = decode_networks(NetworksString),
			defaultProviderId = DefaultProviderId,
			receiptsAllowed = ReceiptsAllowed,
			noRetry = NoRetry,
			defaultValidity = DefaultValidity,
			maxValidity = MaxValidity,
			users = [],
			state = CustState
	},

	k_snmp:set_row(cst, UUID, [
		{cstRPS, Rps},
		{cstPriority, Priority}]),

	Res = k_aaa:set_customer(Id, Customer),
	{ok, {result, io_lib:format("~p", [Res])}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = Id}) ->
	case k_aaa:get_customer_by_system_id(Id) of
		{ok, _Customer = #customer{uuid = UUID}} ->
			k_snmp:del_row(cst, UUID),
			Res = k_aaa:del_customer(Id),
			{ok, {result, io_lib:format("~p", [Res])}, State};
		Error ->
			{ok, {error, io_lib:format("~p", [Error])}, State}
	end.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

prepare(ItemList) when is_list(ItemList) ->
	prepare(ItemList, []);
prepare(Item) ->
	prepare([Item], []).

prepare([], Acc) ->
	{ok, Acc};
prepare([{UUID, Customer = #customer{}} | Rest], Acc) ->
	 #customer{
		allowedSources = OriginatorsList,
		defaultSource = DefaultSource, %addr() | undefined, ????????
		users = UsersList
			} = Customer,

	%% convert users records to proplists
	UserFun = ?record_to_proplist(user),
	UsersPropList = lists:map(fun(User)->
		UserPropList = UserFun(User),
		proplists:delete(pswd_hash, UserPropList)
		end, UsersList),

	%% originators constuctor
	AddrFun = ?record_to_proplist(addr),

	OriginatorsPropList = lists:map(fun(Originator)->
		AddrFun(Originator)
		end, OriginatorsList),

    %% MSISDNS constructor
	UserID = undefined,
    {ok, MSISDNSPropLists} =
    case k_addr2cust:available_addresses(UUID, UserID) of
		{ok, []} -> {ok, null};
	    {ok, MSISDNSList} ->
			{ok, lists:map(fun(MSISDN)->
				AddrFun(MSISDN)
			end, MSISDNSList)}
	end,


	%% defaultSource field validation
	DefSourcePropList =
		case DefaultSource of
			undefined ->
				undefined;
			Record when is_tuple(Record) ->
				AddrFun(Record)
		end,

	%% preparation customer's record
	CustomerFun = ?record_to_proplist(customer),
	CustomerPropList = CustomerFun(Customer#customer{
								users = UsersPropList,
								allowedSources = OriginatorsPropList,
								defaultSource = DefSourcePropList
											}) ++ [{msisdns, MSISDNSPropLists}],
	?log_debug("CustomerPropList: ~p", [CustomerPropList]),
	prepare(Rest, [CustomerPropList | Acc]).


%% convert "addr,ton,npi;addr,ton,npi" to [#addr{}]
decode_addr(AddrString) ->
	AddrList = string:tokens(AddrString, ";"),
	lists:map(fun(Source)->
		[Addr, Ton, Npi] = string:tokens(Source, ","),
		#addr{
			addr = Addr,
			%%% Roma. Here badarg exception may occure if Value contains a bad representation of an integer
			ton = list_to_integer(Ton),
			npi = list_to_integer(Npi)
		}
	end, AddrList).

%% convert "uuid1,uuid2" to ["uuid1", "uuid2"]
decode_networks(NetworksString) ->
	string:tokens(NetworksString, ",").
