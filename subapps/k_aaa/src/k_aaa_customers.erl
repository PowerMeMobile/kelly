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
	AllowedSourcesDocList = [ bson:document(
		[{addr, Addr#addr.addr},
		{ton, Addr#addr.ton},
		{npi, Addr#addr.npi}]
	) || Addr <- Customer#customer.allowed_sources],

	DefaultSourceDoc = case Customer#customer.default_source of
		undefined -> undefined;
		Addr = #addr{} ->
			bson:document(
				[{addr, Addr#addr.addr},
				{ton, Addr#addr.ton},
				{npi, Addr#addr.npi}])
	end,

	UsersDocList =  [ bson:document(
		[{id, User#user.id},
		{pswd_hash, User#user.pswd_hash},
		{permitted_smpp_types, User#user.permitted_smpp_types}]
	) || User <- Customer#customer.users],

	Plist = [
		{id, Customer#customer.id},
		{uuid, Customer#customer.uuid},
		{name, Customer#customer.name},
		{priority, Customer#customer.priority},
		{rps, Customer#customer.rps},
		{allowed_sources, AllowedSourcesDocList},
		{default_source, DefaultSourceDoc},
		{networks, Customer#customer.networks},
		{default_provider_id, Customer#customer.default_provider_id},
		{receipts_allowed, Customer#customer.receipts_allowed},
		{no_retry, Customer#customer.no_retry},
		{default_validity, Customer#customer.default_validity},
		{max_validity, Customer#customer.max_validity},
		{users, UsersDocList},
		{billing_type, Customer#customer.billing_type},
		{state, Customer#customer.state}
	],
	mongodb_storage:upsert(?customerStorageName, [{'_id', CustomerId}], Plist).

-spec get_customers() -> {ok, [{customer_id(), #customer{}}]} | {error, term()}.
get_customers() ->
	case mongodb_storage:find(?customerStorageName, []) of
		{ok, List} ->
			{ok, [
				{Id, proplist_to_record(Plist)} || {Id, Plist} <- List
			]};
		Error ->
			Error
	end.

-spec get_customer(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer(CustomerId) ->
	case mongodb_storage:find_one(?customerStorageName, [{'_id', CustomerId}]) of
		{ok, Plist} when is_list(Plist) ->
			{ok, proplist_to_record(Plist)};
		Error ->
			Error
	end.

-spec get_customer_by_system_id(binary()) -> {ok, #customer{}} | any().
get_customer_by_system_id(SystemId) ->
	case mongodb_storage:find_one(?customerStorageName, [{'id', SystemId}]) of
		{ok, Plist} when is_list(Plist) ->
			{ok, proplist_to_record(Plist)};
		Error ->
			Error
	end.

-spec del_customer(customer_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerId) ->
	mongodb_storage:delete(?customerStorageName, [{'_id', CustomerId}]).

%% ===================================================================
%% Internals
%% ===================================================================

proplist_to_record(Plist) ->
	UsersDocList = proplists:get_value(users, Plist),
	UsersPList = [bson:fields(Doc) || Doc <- UsersDocList],
	Users = [
		#user{
			id = proplists:get_value(id, User),
			pswd_hash = proplists:get_value(pswd_hash, User),
			permitted_smpp_types = proplists:get_value(permitted_smpp_types, User)
		}
		|| User <- UsersPList],

	AllowedSourcesDocList = proplists:get_value(allowed_sources, Plist),
	AllowedSourcesPList = [bson:fields(Doc) || Doc <- AllowedSourcesDocList],
	AllowedSources = [
		#addr{
			addr = proplists:get_value(addr, Addr),
			ton = proplists:get_value(ton, Addr),
			npi = proplists:get_value(npi, Addr)
		}
		|| Addr <- AllowedSourcesPList],

	DefaultSource = case proplists:get_value(default_source, Plist) of
		undefined -> undefined;
		AddrDoc when is_tuple(AddrDoc) ->
			AddrPList = bson:fields(AddrDoc),
			#addr{
				addr = proplists:get_value(addr, AddrPList),
				ton = proplists:get_value(ton, AddrPList),
				npi = proplists:get_value(npi, AddrPList)
			}
	end,

	ID = proplists:get_value(id, Plist),
	UUID = proplists:get_value(uuid, Plist),
	Name = proplists:get_value(name, Plist),
	Priority = proplists:get_value(priority, Plist),
	RPS = proplists:get_value(rps, Plist),
	NetworkIds = proplists:get_value(networks, Plist),
	DefProviderID = proplists:get_value(default_provider_id, Plist),
	ReceiptsAllowed = proplists:get_value(receipts_allowed, Plist),
	NoRetry = proplists:get_value(no_retry, Plist),
	DefValidity = proplists:get_value(default_validity, Plist),
	MaxValidity = proplists:get_value(max_validity, Plist),
	BillingType = proplists:get_value(billing_type, Plist),
	State = proplists:get_value(state, Plist),

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
