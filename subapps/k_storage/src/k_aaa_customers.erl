-module(k_aaa_customers).

%% API
-export([
	get_customers/0,
	get_customer_by_uuid/1,
	get_customer_by_id/1,
	set_customer/2,
	del_customer/1
]).

-include("storages.hrl").
-include("customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_uuid(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerUUID, Customer) ->
	AllowedSourcesDocList = [
		{'addr' , Addr#addr.addr , 'ton' , Addr#addr.ton , 'npi' , Addr#addr.npi}
		|| Addr <- Customer#customer.allowed_sources
	],

	DefaultSourceDoc =
		case Customer#customer.default_source of
			undefined -> undefined;
			Addr = #addr{} ->
				{'addr' , Addr#addr.addr , 'ton' , Addr#addr.ton , 'npi' , Addr#addr.npi}
		end,

	UsersDocList = [
		{
			'id' , User#user.id,
			'password'  , User#user.password,
			'bind_types', lists:map(fun bsondoc:atom_to_binary/1, User#user.bind_types)
		}
		|| User <- Customer#customer.users
	],

	Modifier = {
		'$set' , {
			'customer_id'         , Customer#customer.customer_id,
			'name'                , Customer#customer.name,
			'priority'            , Customer#customer.priority,
			'rps'                 , Customer#customer.rps,
			'allowed_sources'     , AllowedSourcesDocList,
			'default_source'      , DefaultSourceDoc,
			'networks'            , Customer#customer.networks,
			'default_provider_id' , Customer#customer.default_provider_id,
			'receipts_allowed'    , Customer#customer.receipts_allowed,
			'no_retry'            , Customer#customer.no_retry,
			'default_validity'    , Customer#customer.default_validity,
			'max_validity'        , Customer#customer.max_validity,
			'users'               , UsersDocList,
			'billing_type'        , bsondoc:atom_to_binary(Customer#customer.billing_type),
			'state'               , Customer#customer.state
		}
	},
	mongodb_storage:upsert(static_storage, customers, {'_id', CustomerUUID}, Modifier).

-spec get_customers() -> {ok, [{customer_uuid(), #customer{}}]} | {error, term()}.
get_customers() ->
	case mongodb_storage:find(static_storage, customers, {}) of
		{ok, List} ->
			{ok, [
				{Id, doc_to_record(Doc)} || {Id, Doc} <- List
			]};
		Error ->
			Error
	end.

-spec get_customer_by_uuid(customer_uuid()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_uuid(CustomerUUID) ->
	case mongodb_storage:find_one(static_storage, customers, {'_id', CustomerUUID}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec get_customer_by_id(customer_id()) -> {ok, #customer{}} | any().
get_customer_by_id(CustomerId) ->
	case mongodb_storage:find_one(static_storage, customers, {'customer_id', CustomerId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec del_customer(customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerUUID) ->
	mongodb_storage:delete(static_storage, customers, {'_id', CustomerUUID}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
	UsersDocs = bsondoc:at(users, Doc),
	Users = [
		#user{
			id = bsondoc:at(id, UserDoc),
			password = bsondoc:at(password, UserDoc),
			bind_types = lists:map(fun bsondoc:binary_to_atom/1, bsondoc:at(bind_types, UserDoc))
		} || UserDoc <- UsersDocs
	],

	AllowedSourcesDocs = bsondoc:at(allowed_sources, Doc),
	AllowedSources = [
		#addr{
			addr = bsondoc:at(addr, Addr),
			ton = bsondoc:at(ton, Addr),
			npi = bsondoc:at(npi, Addr)
		}
		|| Addr <- AllowedSourcesDocs],

	DefaultSource =
		case bsondoc:at(default_source, Doc) of
			undefined -> undefined;
			AddrDoc when is_tuple(AddrDoc) ->
				#addr{
					addr = bsondoc:at(addr, AddrDoc),
					ton = bsondoc:at(ton, AddrDoc),
					npi = bsondoc:at(npi, AddrDoc)
				}
		end,

	CustomerUUID = bsondoc:at('_id', Doc),
	CustomerId = bsondoc:at(customer_id, Doc),
	Name = bsondoc:at(name, Doc),
	Priority = bsondoc:at(priority, Doc),
	RPS = bsondoc:at(rps, Doc),
	NetworkIds = bsondoc:at(networks, Doc),
	DefProviderId = bsondoc:at(default_provider_id, Doc),
	ReceiptsAllowed = bsondoc:at(receipts_allowed, Doc),
	NoRetry = bsondoc:at(no_retry, Doc),
	DefValidity = bsondoc:at(default_validity, Doc),
	MaxValidity = bsondoc:at(max_validity, Doc),
	BillingType = bsondoc:binary_to_atom(bsondoc:at(billing_type, Doc)),
	State = bsondoc:at(state, Doc),

 	#customer{
		customer_uuid = CustomerUUID,
		customer_id = CustomerId,
		name = Name,
		priority = Priority,
		rps = RPS,
		allowed_sources = AllowedSources,
		default_source = DefaultSource,
		networks = NetworkIds,
		default_provider_id = DefProviderId,
		receipts_allowed = ReceiptsAllowed,
		no_retry = NoRetry,
		default_validity = DefValidity,
		max_validity = MaxValidity,
		users = Users,
		billing_type = BillingType,
		state = State
	}.
