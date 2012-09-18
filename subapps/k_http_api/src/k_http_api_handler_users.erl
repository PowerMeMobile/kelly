-module(k_http_api_handler_users).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

%% export helpers
-export([
	prepare_users/1
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->

	Read = [#method_spec{
				path = [<<"customers">>, customer_id, <<"users">>, id],
				params = [#param{name = customer_id, mandatory = true, repeated = false, type = binary_uuid},
						  #param{name = id, mandatory = true, repeated = false, type = binary}]},
			#method_spec{
				path = [<<"customers">>, customer_id, <<"users">>],
				params = [#param{name = customer_id, mandatory = true, repeated = false, type = binary_uuid}]}],

	UpdateParams = [
		#param{name = customer_id, mandatory = true, repeated = false, type = binary_uuid},
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = pswd, mandatory = false, repeated = false, type = binary},
		#param{name = smpp_types, mandatory = false, repeated = true, type = smpp_type}
	],
	Update = #method_spec{
				path = [<<"customers">>, customer_id, <<"users">>, id],
				params = UpdateParams},

	DeleteParams = [
		#param{name = customer_id, mandatory = true, repeated = false, type = binary_uuid},
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Delete = #method_spec{
				path = [<<"customers">>, customer_id, <<"users">>, id],
				params = DeleteParams},

	CreateParams = [
		#param{name = customer_id, mandatory = true, repeated = false, type = binary_uuid},
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = pswd, mandatory = true, repeated = false, type = binary},
		#param{name = smpp_types, mandatory = true, repeated = true, type = smpp_type}
	],
	Create = #method_spec{
				path = [<<"customers">>, customer_id, <<"users">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = Update,
			delete = Delete
		}}.

create(Params) ->
	CustID = ?gv(customer_id, Params),
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		create_user(Customer, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read(Params) ->
	CustID = ?gv(customer_id, Params),
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		get_customer_user(Customer, ?gv(id, Params));
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

update(Params) ->
	CustID = ?gv(customer_id, Params),
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		update_user(Customer, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

delete(Params) ->
	CustID = ?gv(customer_id, Params),
	UserID = ?gv(id, Params),
	case k_aaa:del_customer_user(CustID, UserID) of
		{error, no_entry} ->
			?log_warn("Customer [~p] not found", [CustID]),
			{exception, 'svc0003'};
		ok ->
			{http_code, 204};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

%% ===================================================================
%% Local Functions
%% ===================================================================

translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(permitted_smpp_types) ->
	smpp_types;
translate_name(Name) ->
	Name.

prepare_users(Users) ->
	UserFun = ?record_to_proplist(user),
	{ok, lists:map(fun(User)->
		UserPropList = UserFun(User),
		translate(proplists:delete(pswd_hash, UserPropList))
		end, Users)}.

create_user(Customer, Params) ->
	UserID = ?gv(id, Params),
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, #user{}} ->
			{exception, 'svc0004'};
		{error, no_entry} ->
			Pass = ?gv(pswd, Params),
			SMPPTypes = ?gv(smpp_types, Params),
			User = #user{
				id 						= UserID,
				pswd_hash 				= crypto:sha(Pass),
				permitted_smpp_types 	= SMPPTypes
				},
			ok = k_aaa:set_customer_user(User, Customer#customer.uuid),
			{ok, UserPropList} = prepare_users([User]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{http_code, 201, UserPropList};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

update_user(Customer, Params) ->
	UserID = ?gv(id, Params),
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, User} ->
			Updated = #user{
				id = UserID,
				pswd_hash = resolve_pass(?gv(pswd, Params), User#user.pswd_hash),
				permitted_smpp_types = resolve(smpp_types, Params, User#user.permitted_smpp_types)
			},
			ok = k_aaa:set_customer_user(Updated, Customer#customer.uuid),
			{ok, [UserPropList]} = prepare_users([Updated]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{ok, UserPropList};
   	  	{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

get_customer_user(Customer, undefined) ->
	#customer{users = Users} = Customer,
	{ok, UserPropList} = prepare_users(Users),
	?log_debug("UserPropList: ~p", [UserPropList]),
	{ok, UserPropList};
get_customer_user(Customer, UserID) ->
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, User} ->
			{ok, [UserPropList]} = prepare_users([User]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{ok, UserPropList};
   	  	{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

resolve_pass(undefined, Pass) ->
	Pass;
resolve_pass(NewPass, _Pass) ->
	crypto:sha(NewPass).
