-module(k_http_api_handler_customers).

-behaviour(gen_cowboy_crud).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("crud_specs.hrl").

init() ->

	Read = [#method_spec{
				path = [<<"customers">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = string_uuid}]},
			#method_spec{
				path = [<<"customers">>],
				params = []}],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = system_id, mandatory = false, repeated = false, type = string},
		#param{name = name, mandatory = false, repeated = false, type = string},
		#param{name = rps, mandatory = false, repeated = false, type = disabled},
		#param{name = originator, mandatory = false, repeated = true, type = addr},
		#param{name = default_originator, mandatory = false, repeated = false, type = addr},
		#param{name = network, mandatory = false, repeated = true, type = string_uuid},
		#param{name = default_provider_id, mandatory = false, repeated = false, type = string_uuid},
		#param{name = receipts_allowed, mandatory = false, repeated = false, type = boolean},
		#param{name = default_validity, mandatory = false, repeated = false, type = string},
		#param{name = max_validity, mandatory = false, repeated = false, type = integer}
	],
	Update = #method_spec{
				path = [<<"customers">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = string_uuid}
	],
	Delete = #method_spec{
				path = [<<"customers">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = string_uuid},
		#param{name = system_id, mandatory = true, repeated = false, type = string},
		#param{name = name, mandatory = true, repeated = false, type = string},
		#param{name = rps, mandatory = false, repeated = false, type = disabled},
		#param{name = originator, mandatory = true, repeated = true, type = addr},
		#param{name = default_originator, mandatory = true, repeated = false, type = addr},
		#param{name = network, mandatory = true, repeated = true, type = string_uuid},
		#param{name = default_provider_id, mandatory = true, repeated = false, type = string_uuid},
		#param{name = receipts_allowed, mandatory = true, repeated = false, type = boolean},
		#param{name = default_validity, mandatory = true, repeated = false, type = string},
		#param{name = max_validity, mandatory = true, repeated = false, type = integer}
	],
	Create = #method_spec{
				path = [<<"customers">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = Update,
			delete = Delete
		}}.

create(Params) ->
	case ?gv(id, Params) of
		undefined ->
			UUID = k_uuid:to_string(k_uuid:newid()),
			create_customer(lists:keyreplace(id, 1, Params, {id, UUID}));
		_ ->
			is_exist(Params)
	end.

is_exist(Params) ->
	UUID = ?gv(id, Params),
	case k_aaa:get_customer_by_id(UUID) of
		{ok, #customer{}} ->
			{exception, 'svc0004'};
		{error, no_entry} ->
			create_customer(Params);
		Error ->
			?log_debug("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read(Params) ->
	UUID = ?gv(id, Params),
	case UUID of
		undefined ->
			read_all();
		_ ->
			read_id(UUID)
	end.

read_all() ->
	case k_aaa:get_customers() of
 		{ok, CustList} ->
			{ok, CustPropLists} = prepare(CustList),
			?log_debug("CustPropLists: ~p", [CustPropLists]),
			{ok, {customers, CustPropLists}};
		{error, Error} ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read_id(UUID) ->
	case k_aaa:get_customer_by_id(UUID) of
		{ok, Customer = #customer{}} ->
			{ok, [CustPropList]} = prepare({UUID, Customer}),
			?log_debug("CustPropList: ~p", [CustPropList]),
			{ok, CustPropList};
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

update(Params) ->
	UUID = ?gv(id, Params),
	case k_aaa:get_customer_by_id(UUID) of
		{ok, Customer = #customer{}} ->
			update_customer(Customer, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_debug("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.


delete(Params) ->
	UUID = ?gv(id, Params),
	k_snmp:del_row(cst, UUID),
	ok = k_aaa:del_customer(UUID),
	{http_code, 204}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_customer(Customer, Params) ->
	NewRPS = resolve(rps, Params, Customer#customer.rps),
	NewName = resolve(name, Params, Customer#customer.name),
	NewOriginators = resolve(originator, Params, Customer#customer.allowedSources),
	NewDefaultOriginator = resolve(default_originator, Params, Customer#customer.defaultSource),
	NewNetworks = resolve(networks, Params, Customer#customer.networks),
	NewDefaultProviderId = resolve(default_provider_id, Params, Customer#customer.defaultProviderId),
	NewReceiptsAllowed = resolve(receipts_allowed, Params, Customer#customer.receiptsAllowed),
	NewDefaultValidity = resolve(default_validity, Params, Customer#customer.defaultValidity),
	NewMaxValidity = resolve(max_validity, Params, Customer#customer.maxValidity),
	NewCustomer = #customer{
		id = Customer#customer.id,
		uuid = Customer#customer.uuid,
		name = NewName,
		priority = 0,
		rps = NewRPS,
		allowedSources = NewOriginators,
		defaultSource = NewDefaultOriginator,
		networks = NewNetworks,
		defaultProviderId = NewDefaultProviderId,
		receiptsAllowed = NewReceiptsAllowed,
		noRetry = false,
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
	{http_code, 200, CustPropList}.

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.

create_customer(Params) ->
	UUID = ?gv(id, Params),
	RPS = ?gv(rps, Params),
	System_id = ?gv(system_id, Params),
	Customer = #customer{
		id = System_id,
		uuid = UUID,
		name = ?gv(name, Params),
		priority = 0,
		rps = RPS,
		allowedSources = ?gv(originator, Params),
		defaultSource = ?gv(default_originator, Params),
		networks = ?gv(network, Params),
		defaultProviderId = ?gv(default_provider_id, Params),
		receiptsAllowed = ?gv(receipts_allowed, Params),
		noRetry = false,
		defaultValidity = ?gv(default_validity, Params),
		maxValidity = ?gv(max_validity, Params),
		users = []
	},
	%% k_snmp:set_row(cst, UUID, [
	%% 	{cstRPS, RPS},
	%% 	{cstPriority, Priority}]),
	ok = k_aaa:set_customer(System_id, Customer),
	{ok, [CustPropList]} = prepare({UUID, Customer}),
	?log_debug("CustPropList: ~p", [CustPropList]),
	{http_code, 201, CustPropList}.

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
