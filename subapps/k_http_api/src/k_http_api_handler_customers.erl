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
	case k_aaa_api:get_customers() of
 		{ok, CustList} ->
			?log_debug("CustList: ~p", [CustList]),
			{ok, CustReady} = prepare(CustList),
			?log_debug("CustReady: ~p", [CustReady]),
			{ok, {customers, CustReady}, State};
		{error, Error} ->
			{ok, Error, State}
	end;

handle(_Req, #get{}, State = #state{id = Id}) ->
	case k_aaa_api:get_customer_by_system_id(Id) of
		{ok, Customer = #customer{uuid = UUID}} ->
			{ok, [CustReady]} = prepare({UUID, Customer}),
			?log_debug("CustReady: ~p", [CustReady]),
			{ok, CustReady, State};
		Any ->
			{ok, Any, State}
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

	Res = k_aaa_api:set_customer(Id, Customer),
	{ok, {result, Res}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{id = Id}) ->
	case k_aaa_api:get_customer_by_system_id(Id) of
		{ok, _Customer = #customer{uuid = UUID}} ->
			k_snmp:del_row(cst, UUID),
			Res = k_aaa_api:del_customer(Id),
			{ok, {result, Res}, State};
		Error ->
			{ok, {result, Error}, State}
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
		allowedSources = AddrsList,
		defaultSource = DefaultSource, %addr() | undefined, ????????
		users = UsersList,
		networks = NtwList
			} = Customer,

	%% preparation users' records
	UserFun = ?record_to_proplist(user),
	UsersPropList = lists:map(fun(User = #user{permitted_smpp_types = Types})->
		TypesPropList = lists:map(fun(Type) -> {type, Type} end, Types),
		{RecName, PropList} = UserFun(User#user{permitted_smpp_types = TypesPropList}),
		{RecName, proplists:delete(pswd_hash, PropList)}
		end, UsersList),

	%% source addrs' constuctor
	AddrFun = ?record_to_proplist(addr),

	AddrsPropList = lists:map(fun(AddrRecord)->
		AddrFun(AddrRecord)
		end, AddrsList),

	%% MSISDNS constructor
	{ok, MSISDNSList} = k_addr2cust:available_addresses(UUID),
	MSISDNS = lists:map(fun(MSISDN)->
		AddrFun(MSISDN)
		end, MSISDNSList),

	%% preparation defaultSource field
	DefSourcePropList =
		case DefaultSource of
			undefined ->
				undefined;
			Record when is_tuple(Record) ->
				AddrFun(Record)
		end,
	%% network constructor
	Ntws = lists:map(fun(NtwUUID) ->
		{network, NtwUUID}
		end, NtwList),


	%% preparation customer's record
	CustomerFun = ?record_to_proplist_(customer),
	ReadyCustomer = CustomerFun(Customer#customer{
								users = UsersPropList,
								allowedSources = AddrsPropList,
								defaultSource = DefSourcePropList,
								networks = Ntws},
								[{msisdns, MSISDNS}]),
	?log_debug("ReadyCustomer: ~p", [ReadyCustomer]),
	prepare(Rest, [ReadyCustomer | Acc]).


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
