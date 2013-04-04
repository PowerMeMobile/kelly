-module(k_http_api_handler_customers).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/customer.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

init() ->

	Read = [#method_spec{
				path = [<<"customers">>, id],
				params = [#param{name = id, mandatory = true, repeated = false, type = binary}]},
			#method_spec{
				path = [<<"customers">>],
				params = []}],

	UpdateParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = system_id, mandatory = false, repeated = false, type = binary},
		#param{name = name, mandatory = false, repeated = false, type = binary},
		#param{name = priority, mandatory = false, repeated = false, type = disabled},
		#param{name = rps, mandatory = false, repeated = false, type = disabled},
		#param{name = originators, mandatory = false, repeated = true, type = {custom, fun decode_addr/1}},
		#param{name = default_originator, mandatory = false, repeated = false, type = {custom, fun decode_addr/1}},
		#param{name = networks, mandatory = false, repeated = true, type = binary},
		#param{name = default_provider_id, mandatory = false, repeated = false, type = binary},
		#param{name = receipts_allowed, mandatory = false, repeated = false, type = boolean},
		#param{name = default_validity, mandatory = false, repeated = false, type = binary},
		#param{name = max_validity, mandatory = false, repeated = false, type = integer},
		#param{name = billing_type, mandatory = false, repeated = false, type = {custom, fun billing_type/1}},
		#param{name = state, mandatory = false, repeated = false, type = {custom, fun customer_state/1}}
	],
	Update = #method_spec{
				path = [<<"customers">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Delete = #method_spec{
				path = [<<"customers">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = id, mandatory = false, repeated = false, type = binary},
		#param{name = system_id, mandatory = true, repeated = false, type = binary},
		#param{name = name, mandatory = true, repeated = false, type = binary},
		#param{name = priority, mandatory = false, repeated = false, type = disabled},
		#param{name = rps, mandatory = false, repeated = false, type = disabled},
		#param{name = originators, mandatory = false, repeated = true, type = {custom, fun decode_addr/1}},
		#param{name = default_originator, mandatory = false, repeated = false, type = {custom, fun decode_addr/1}},
		#param{name = networks, mandatory = true, repeated = true, type = binary},
		#param{name = default_provider_id, mandatory = true, repeated = false, type = binary},
		#param{name = receipts_allowed, mandatory = true, repeated = false, type = boolean},
		#param{name = default_validity, mandatory = true, repeated = false, type = binary},
		#param{name = max_validity, mandatory = true, repeated = false, type = integer},
		#param{name = billing_type, mandatory = true, repeated = false, type = {custom, fun billing_type/1}},
		#param{name = state, mandatory = true, repeated = false, type = {custom, fun customer_state/1}}
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
			UUID = uuid:unparse(uuid:generate_time()),
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
	k_snmp:delete_customer(UUID),
	ok = k_aaa:del_customer(UUID),
	{http_code, 204}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

update_customer(Customer, Params) ->
	NewName = ?resolve(name, Params, Customer#customer.name),
	NewOriginators = ?resolve(originators, Params, Customer#customer.allowed_sources),
	NewDefaultOriginator = ?resolve(default_originator, Params, Customer#customer.default_source),
	NewNetworks = ?resolve(networks, Params, Customer#customer.networks),
	NewDefaultProviderId = ?resolve(default_provider_id, Params, Customer#customer.default_provider_id),
	NewReceiptsAllowed = ?resolve(receipts_allowed, Params, Customer#customer.receipts_allowed),
	NewDefaultValidity = ?resolve(default_validity, Params, Customer#customer.default_validity),
	NewMaxValidity = ?resolve(max_validity, Params, Customer#customer.max_validity),
	NewBillingType = ?resolve(billing_type, Params, Customer#customer.billing_type),
	NewState = ?resolve(state, Params, Customer#customer.state),
	NewCustomer = #customer{
		id = Customer#customer.id,
		uuid = Customer#customer.uuid,
		name = NewName,
		priority = 1,
		rps = 10000,
		allowed_sources = NewOriginators,
		default_source = NewDefaultOriginator,
		networks = NewNetworks,
		default_provider_id = NewDefaultProviderId,
		receipts_allowed = NewReceiptsAllowed,
		no_retry = false,
		default_validity = NewDefaultValidity,
		max_validity = NewMaxValidity,
		users = Customer#customer.users,
		billing_type = NewBillingType,
		state = NewState
	},
	ok = k_aaa:set_customer(Customer#customer.id, NewCustomer),
	{ok, [CustPropList]} = prepare({Customer#customer.uuid, NewCustomer}),
	?log_debug("CustPropList: ~p", [CustPropList]),
	{http_code, 200, CustPropList}.

create_customer(Params) ->
	UUID = ?gv(id, Params),
	Priority = 1,
	RPS = 10000,
	System_id = ?gv(system_id, Params),
	Customer = #customer{
		id = System_id,
		uuid = UUID,
		name = ?gv(name, Params),
		priority = Priority,
		rps = RPS,
		allowed_sources = ?gv(originators, Params),
		default_source = ?gv(default_originator, Params),
		networks = ?gv(networks, Params),
		default_provider_id = ?gv(default_provider_id, Params),
		receipts_allowed = ?gv(receipts_allowed, Params),
		no_retry = false,
		default_validity = ?gv(default_validity, Params),
		max_validity = ?gv(max_validity, Params),
		users = [],
		billing_type = ?gv(billing_type, Params),
		state = ?gv(state, Params)
	},
	k_snmp:set_customer(UUID, RPS, Priority),
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
prepare([{UUID, Customer = #customer{}} | Rest], Acc) ->
	 #customer{
		allowed_sources = OriginatorsList,
		default_source = DefaultSource,
		users = UsersList
	} = Customer,

	{ok, UsersPropList} = k_http_api_handler_users:prepare_users(UsersList),

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
								allowed_sources = OriginatorsPropList,
								default_source = DefSourcePropList
											}),
	UUIDBinStr = UUID,
	DefaultProviderIDBinStr = ?gv(default_provider_id, CustomerPropList),
	NetworksBinStr = ?gv(networks, CustomerPropList),
	Renamed = translate(CustomerPropList),
	ConvertedID = lists:keyreplace(id, 1, Renamed, {id, UUIDBinStr}),
	ConvertedDefaultProviderID = lists:keyreplace(default_provider_id, 1, ConvertedID, {default_provider_id, DefaultProviderIDBinStr}),
	ConvertedNetworks = lists:keyreplace(networks, 1, ConvertedDefaultProviderID, {networks, NetworksBinStr}),

	?log_debug("CustomerPropList: ~p", [CustomerPropList]),
	prepare(Rest, [ConvertedNetworks | Acc]).


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
translate_name(allowed_sources) ->
	originators;
translate_name(default_source) ->
	default_originator;
translate_name(default_provider_id) ->
	default_provider_id;
translate_name(receipts_allowed) ->
	receipts_allowed;
translate_name(no_retry) ->
	no_retry;
translate_name(default_validity) ->
	default_validity;
translate_name(max_validity) ->
	max_validity;
translate_name(Name) ->
	Name.

%% convert "addr,ton,npi" to #addr{addr, ton, npi}
decode_addr(AddrBin) ->
	AddrString = binary_to_list(AddrBin),
	[Addr, Ton, Npi] = string:tokens(AddrString, ","),
	#addr{
		addr = list_to_binary(Addr),
		ton = list_to_integer(Ton),
		npi = list_to_integer(Npi)
	}.

customer_state(StateBin) ->
	case StateBin of
		<<"0">> -> 0;
		<<"1">> -> 1
	end.

billing_type(TypeBin) ->
	case TypeBin of
		<<"prepaid">> -> prepaid;
		<<"postpaid">> -> postpaid
	end.
