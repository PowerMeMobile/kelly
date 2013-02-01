-module(k_aaa_customers).

%% API
-export([
	get_customers/0,
	get_customer/1,
	get_customer_by_system_id/1,
	set_customer/2,
	del_customer/1
]).

-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_id(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerId, Customer) ->
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
		{'id' , User#user.id, 'pswd_hash' , User#user.pswd_hash, 'permitted_smpp_types' , User#user.permitted_smpp_types}
		|| User <- Customer#customer.users
	],

	Modifier = {
		'$set' , {
			'id'                  , Customer#customer.id,
			'uuid'                , Customer#customer.uuid,
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
			'billing_type'        , Customer#customer.billing_type,
			'state'               , Customer#customer.state
		}
	},
	mongodb_storage:upsert(k_static_storage, customers, {'_id', CustomerId}, Modifier).

-spec get_customers() -> {ok, [{customer_id(), #customer{}}]} | {error, term()}.
get_customers() ->
	case mongodb_storage:find(k_static_storage, customers, {}) of
		{ok, List} ->
			{ok, [
				{Id, doc_to_record(Doc)} || {Id, Doc} <- List
			]};
		Error ->
			Error
	end.

-spec get_customer(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer(CustomerId) ->
	case mongodb_storage:find_one(k_static_storage, customers, {'_id', CustomerId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec get_customer_by_system_id(binary()) -> {ok, #customer{}} | any().
get_customer_by_system_id(SystemId) ->
	case mongodb_storage:find_one(k_static_storage, customers, {'id', SystemId}) of
		{ok, Doc} ->
			{ok, doc_to_record(Doc)};
		Error ->
			Error
	end.

-spec del_customer(customer_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerId) ->
	mongodb_storage:delete(k_static_storage, customers, {'_id', CustomerId}).

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
	UsersDocs = bson:at(users, Doc),
	Users = [
		#user{
			id = bson:at(id, User),
			pswd_hash = bson:at(pswd_hash, User),
			permitted_smpp_types = bson:at(permitted_smpp_types, User)
		}
		|| User <- UsersDocs],

	AllowedSourcesDocs = bson:at(allowed_sources, Doc),
	AllowedSources = [
		#addr{
			addr = bson:at(addr, Addr),
			ton = bson:at(ton, Addr),
			npi = bson:at(npi, Addr)
		}
		|| Addr <- AllowedSourcesDocs],

	DefaultSource =
		case bson:at(default_source, Doc) of
			undefined -> undefined;
			AddrDoc when is_tuple(AddrDoc) ->
				#addr{
					addr = bson:at(addr, AddrDoc),
					ton = bson:at(ton, AddrDoc),
					npi = bson:at(npi, AddrDoc)
				}
		end,

	ID = bson:at(id, Doc),
	UUID = bson:at(uuid, Doc),
	Name = bson:at(name, Doc),
	Priority = bson:at(priority, Doc),
	RPS = bson:at(rps, Doc),
	NetworkIds = bson:at(networks, Doc),
	DefProviderID = bson:at(default_provider_id, Doc),
	ReceiptsAllowed = bson:at(receipts_allowed, Doc),
	NoRetry = bson:at(no_retry, Doc),
	DefValidity = bson:at(default_validity, Doc),
	MaxValidity = bson:at(max_validity, Doc),
	BillingType = bson:at(billing_type, Doc),
	State = bson:at(state, Doc),

 	#customer{
		id = ID,
		uuid = UUID,
		name = Name,
		priority = Priority,
		rps = RPS,
		allowed_sources = AllowedSources,
		default_source = DefaultSource,
		networks = NetworkIds,
		default_provider_id = DefProviderID,
		receipts_allowed = ReceiptsAllowed,
		no_retry = NoRetry,
		default_validity = DefValidity,
		max_validity = MaxValidity,
		users = Users,
		billing_type = BillingType,
		state = State
	}.
