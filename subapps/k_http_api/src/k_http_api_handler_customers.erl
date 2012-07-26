-module(k_http_api_handler_customers).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").

-record(state, {
	id :: list() | all,
	customer :: #customer{}
}).

%%% REST parameters
-record(get, {
}).

-record(create, {
	id = {optional, <<"id">>, string_uuid},
	system_id = {mandatory, <<"system_id">>, list},
	name = {mandatory, <<"name">>, list},
	%% priority = {optional, <<"priority">>, integer}, %% obsolete
	rps = {optional, <<"rps">>, integer},
	originators = {mandatory, <<"originators">>, list},
	default_originator = {mandatory, <<"default_originator">>, list},
	networks = {mandatory, <<"networks">>, list},
	default_provider_id = {mandatory, <<"default_provider_id">>, list},
	receipts_allowed = {mandatory, <<"receipts_allowed">>, boolean},
	%% no_retry = {optional, <<"no_retry">>, boolean}, %% obsolete
	default_validity = {mandatory, <<"default_validity">>, integer},
	max_validity = {mandatory, <<"max_validity">>, integer}
}).

-record(update, {
	system_id = {optional, <<"id">>, list},
	name = {optional, <<"name">>, list},
	%% priority = {optional, <<"priority">>, integer}, %% obsolete
	rps = {optional, <<"rps">>, integer},
	originators = {optional, <<"allowed_sources">>, list},
	default_originator = {optional, <<"default_source">>, list},
	networks = {optional, <<"networks">>, list},
	default_provider_id = {optional, <<"default_provider_id">>, list},
	receipts_allowed = {optional, <<"receipts_allowed">>, boolean},
	%% no_retry = {optional, <<"no_retry">>, boolean}, %% obsolete
	default_validity = {optional, <<"default_validity">>, integer},
	max_validity = {optional, <<"max_validity">>, integer}
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
	case k_aaa:get_customers() of
 		{ok, CustList} ->
			{ok, CustPropLists} = prepare(CustList),
			?log_debug("CustPropLists: ~p", [CustPropLists]),
			{ok, {customers, CustPropLists}, State};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #get{}, State = #state{id = CustSysID}) ->
	case k_aaa:get_customer_by_id(CustSysID) of
		{ok, Customer = #customer{uuid = UUID}} ->
			{ok, [CustPropList]} = prepare({UUID, Customer}),
			?log_debug("CustPropList: ~p", [CustPropList]),
			{ok, {customer, CustPropList}, State};
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

%% customer's `rps' setting is disabled.
%% see http://extranet.powermemobile.com/issues/17465 for detail.
handle(_Req, #create{rps = RPS}, State) when RPS =/= undefined ->
	{http_code, 500, [{error, "RPS setting unavailable in OpenAlley"}], State};
handle(_Req, #update{rps = RPS}, State) when RPS =/= undefined ->
	{http_code, 500, [{error, "RPS setting unavailable in OpenAlley"}], State};

handle(Req, Create = #create{id = undefined}, State) ->
	UUID = k_uuid:to_string(k_uuid:newid()),
	create_customer(Req, Create#create{id = UUID}, State);
handle(Req, Create = #create{id = ID}, State = #state{}) ->
	case k_aaa:get_customer_by_id(ID) of
		{ok, #customer{}} ->
			{exception, 'svc0004', [], State};
		{error, no_entry} ->
			create_customer(Req, Create, State);
		Error ->
			?log_debug("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(Req, Update = #update{}, State = #state{id = ID}) ->
	case k_aaa:get_customer_by_id(ID) of
		{ok, Customer = #customer{}} ->
			update_customer(Req, Update, State#state{customer = Customer});
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_debug("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #delete{}, State = #state{id = UUID}) ->
	case k_aaa:get_customer_by_id(UUID) of
		{ok, #customer{id = SystemID}} ->
			k_snmp:del_row(cst, UUID),
			ok = k_aaa:del_customer(SystemID),
			{http_code, 204, State};
		Error ->
			?log_debug("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end.

terminate(_Req, _State = #state{}) ->
    ok.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_customer(_Req, Update, State = #state{customer = Customer}) ->
	NewRPS = resolve(Update#update.rps, Customer#customer.rps),
	NewName = resolve(Update#update.name, Customer#customer.name),
	NewOriginators = resolve(decode_addresses(Update#update.originators), Customer#customer.allowedSources),
	NewDefaultOriginator = resolve(decode_address(Update#update.default_originator), Customer#customer.defaultSource),
	NewNetworks = resolve(decode_networks(Update#update.networks), Customer#customer.networks),
	NewDefaultProviderId = resolve(Update#update.default_provider_id, Customer#customer.defaultProviderId),
	NewReceiptsAllowed = resolve(Update#update.receipts_allowed, Customer#customer.receiptsAllowed),
	NewDefaultValidity = resolve(Update#update.default_validity, Customer#customer.defaultValidity),
	NewMaxValidity = resolve(Update#update.max_validity, Customer#customer.maxValidity),
	NewCustomer = #customer{
		id = Customer#customer.id,
		uuid = Customer#customer.uuid,
		name = NewName,
		priority = undefined,
		rps = NewRPS,
		allowedSources = NewOriginators,
		defaultSource = NewDefaultOriginator,
		networks = NewNetworks,
		defaultProviderId = NewDefaultProviderId,
		receiptsAllowed = NewReceiptsAllowed,
		noRetry = undefined,
		defaultValidity = NewDefaultValidity,
		maxValidity = NewMaxValidity,
		users = Customer#customer.users
	},
	%% k_snmp:set_row(cst, Customer#customer.uuid, [
	%% 	{cstRPS, NewRPS},
	%% 	{cstPriority, Priority}]),
	ok = k_aaa:set_customer(Customer#customer.id, NewCustomer),
	{ok, [CustPropList]} = prepare({Customer#customer.uuid, NewCustomer}),
	?log_debug("CustPropList: ~p", [CustPropList]),
	{http_code, 200, CustPropList, State}.

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.

create_customer(_Req, Create, State) ->
	UUID = Create#create.id,
	RPS = Create#create.rps,
	System_id = Create#create.system_id,
	Customer = #customer{
		id = System_id,
		uuid = UUID,
		name = Create#create.name,
		priority = undefined,
		rps = RPS,
		allowedSources = decode_addresses(Create#create.originators),
		defaultSource = decode_address(Create#create.default_originator),
		networks = decode_networks(Create#create.networks),
		defaultProviderId = Create#create.default_provider_id,
		receiptsAllowed = Create#create.receipts_allowed,
		noRetry = undefined,
		defaultValidity = Create#create.default_validity,
		maxValidity = Create#create.max_validity,
		users = []
	},
	%% k_snmp:set_row(cst, UUID, [
	%% 	{cstRPS, RPS},
	%% 	{cstPriority, Priority}]),
	ok = k_aaa:set_customer(System_id, Customer),
	{ok, [CustPropList]} = prepare({UUID, Customer}),
	?log_debug("CustPropList: ~p", [CustPropList]),
	{http_code, 201, CustPropList, State}.

%% snmp_update(UUID, RPS, Priority) ->
%% 	Parameters = lists:filter(fun({_ColumnName, Value}) ->
%% 		case Value of
%% 			undefined -> false;
%% 			_ -> true
%% 		end
%% 	end, [{cstRPS, RPS}, {cstPriority, Priority}]),
%% 	?log_debug("Parameters: ~p", [Parameters]),
%% 	k_snmp:set_row(cst, UUID, Parameters).

prepare(ItemList) when is_list(ItemList) ->
	prepare(ItemList, []);
prepare(Item) ->
	prepare([Item], []).

prepare([], Acc) ->
	{ok, Acc};
prepare([{_UUID, Customer = #customer{}} | Rest], Acc) ->
	 #customer{
		allowedSources = OriginatorsList,
		defaultSource = DefaultSource,
		users = UsersList
	} = Customer,

	%% convert users records to proplists
	UserFun = ?record_to_proplist(user),
	UsersPropList = lists:map(
		fun(User)->
			UserPropList = UserFun(User),
			proplists:delete(pswd_hash, UserPropList)
		end, UsersList),

	%% originators constructor
	AddrFun = ?record_to_proplist(addr),

	OriginatorsPropList = lists:map(
		fun(Originator)->
			AddrFun(Originator)
		end, OriginatorsList),

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
											}),
	?log_debug("CustomerPropList: ~p", [CustomerPropList]),
	prepare(Rest, [translate(CustomerPropList) | Acc]).

%% convert "addr,ton,npi;addr,ton,npi" to [#addr{}]
decode_address(undefined) ->
	undefined;
decode_address(AddrString) ->
	[Addr] = decode_addresses(AddrString),
	Addr.

decode_addresses(undefined) ->
	undefined;
decode_addresses(AddrString) ->
	AddrList = string:tokens(AddrString, ";"),
	lists:map(fun(Source)->
		[Addr, Ton, Npi] = string:tokens(Source, ","),
		#addr{
			addr = Addr,
			%%% Here badarg exception may occure if Value contains a bad representation of an integer
			ton = list_to_integer(Ton),
			npi = list_to_integer(Npi)
		}
	end, AddrList).

%% convert "uuid1,uuid2" to ["uuid1", "uuid2"]
decode_networks(undefined) ->
	undefined;
decode_networks(NetworksString) ->
	string:tokens(NetworksString, ",").

translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(id) ->
	system_id;
translate_name(uuid) ->
	id;
translate_name(allowedSources) ->
	originators;
translate_name(defaultSource) ->
	default_originator;
translate_name(defaultProviderId) ->
	default_provider_id;
translate_name(receiptsAllowed) ->
	receipts_allowed;
translate_name(noRetry) ->
	no_retry;
translate_name(defaultValidity) ->
	default_validity;
translate_name(maxValidity) ->
	max_validity;
translate_name(Name) ->
	Name.
