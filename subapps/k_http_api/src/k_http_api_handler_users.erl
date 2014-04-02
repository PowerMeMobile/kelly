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

-include_lib("alley_common/include/utils.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
	Read = [
		#param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
		#param{name = id, mandatory = false, repeated = false, type = binary}
	],
	Update = [
		#param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = password, mandatory = false, repeated = false, type = binary},
		#param{name = connection_types, mandatory = false, repeated = true, type =
             {custom, fun connection_type/1}}
	],
	Delete = [
		#param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
		#param{name = id, mandatory = true, repeated = false, type = binary}
	],
	Create = [
		#param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
		#param{name = id, mandatory = true, repeated = false, type = binary},
		#param{name = password, mandatory = true, repeated = false, type = binary},
		#param{name = connection_types, mandatory = true, repeated = true, type =
            {custom, fun connection_type/1}}
	],
	{ok, #specs{
		create = Create,
		read = Read,
		update = Update,
		delete = Delete,
		route = "/customers/:customer_uuid/users/[:id]"
	}}.

create(Params) ->
	CustomerUUID = ?gv(customer_uuid, Params),
	case k_aaa:get_customer_by_uuid(CustomerUUID) of
		{ok, Customer = #customer{}} ->
	   		create_user(Customer, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

read(Params) ->
	CustomerUUID = ?gv(customer_uuid, Params),
	case k_aaa:get_customer_by_uuid(CustomerUUID) of
		{ok, Customer = #customer{}} ->
	   		get_customer_user(Customer, ?gv(id, Params));
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

update(Params) ->
	CustomerUUID = ?gv(customer_uuid, Params),
	case k_aaa:get_customer_by_uuid(CustomerUUID) of
		{ok, Customer = #customer{}} ->
	   		update_user(Customer, Params);
		{error, no_entry} ->
			{exception, 'svc0003'};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

delete(Params) ->
	CustomerUUID = ?gv(customer_uuid, Params),
	UserID = ?gv(id, Params),
	case k_aaa:del_customer_user(CustomerUUID, UserID) of
		{error, no_entry} ->
			?log_warn("Customer [~p] not found", [CustomerUUID]),
			{exception, 'svc0003'};
		ok ->
			{http_code, 204};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500}
	end.

-spec prepare_users(#user{}) -> {ok, [{atom(), term()}]}.
prepare_users(User = #user{}) ->
	UserFun = ?record_to_proplist(user),
	UserPropList = UserFun(User),
	proplists:delete(password, UserPropList);
prepare_users(Users) when is_list(Users) ->
	{ok, [prepare_users(User) || User <- Users]}.

%% ===================================================================
%% Local Functions
%% ===================================================================


create_user(Customer, Params) ->
	UserID = ?gv(id, Params),
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, #user{}} ->
			{exception, 'svc0004'};
		{error, no_entry} ->
			Password = ?gv(password, Params),
			ConnectionTypes = ?gv(connection_types, Params),
			User = #user{
				id = UserID,
				password = base64:encode(crypto:sha(Password)),
				connection_types = ConnectionTypes
			},
			ok = k_aaa:set_customer_user(User, Customer#customer.customer_uuid),
			{ok, [UserPropList]} = prepare_users([User]),
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
				password = resolve_pass(?gv(password, Params), User#user.password),
				connection_types = ?gv(connection_types, Params, User#user.connection_types)
			},
			ok = k_aaa:set_customer_user(Updated, Customer#customer.customer_uuid),
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
	base64:encode(crypto:sha(NewPass)).

connection_type(Type) ->
	case Type of
        <<"mm">> -> mm;
        <<"soap">> -> soap;
		<<"oneapi">> -> oneapi;
		<<"transmitter">> -> transmitter;
		<<"receiver">> -> receiver;
		<<"transceiver">> -> transceiver
	end.
