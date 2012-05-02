-module(k_aaa_api).

%% API
-export([
	set_customer/2,
	del_customer/1,
	get_customer_by_system_id/1,
	get_customer_by_id/1,
	get_customer_user/2,
	set_customer_user/2,
	del_customer_user/2
]).

-include("application.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% Customer API
%% ===================================================================

-spec set_customer(system_id(), #customer{}) -> ok | {error, term()}.
set_customer(SystemId, Customer = #customer{
	uuid = CustomerId
}) ->
	case k_aaa_sid_to_cid:sid_to_cid(SystemId, CustomerId) of
		ok ->
			k_aaa_customers:set_customer(CustomerId, Customer);
		Error ->
			Error
	end.

-spec get_customer_by_system_id(system_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_system_id(SystemId) ->
	case k_aaa_sid_to_cid:cid_by_sid(SystemId) of
		{ok, CustomerId} ->
			k_aaa_customers:get_customer(CustomerId);
		Error ->
			Error
	end.

-spec get_customer_by_id(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_id(CustomerId) ->
	k_aaa_customers:get_customer(CustomerId).

-spec del_customer(customer_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerId) ->
	k_aaa_customers:del_customer(CustomerId).

-spec get_customer_user(#customer{}, string()) -> {ok, #user{}} | {error, no_entry} | {error, term()}.
get_customer_user(#customer{users = UserList}, UserName) ->
	find_user(UserList, UserName).

-spec set_customer_user(#user{}, customer_id()) -> ok | {error, no_entry} | {error, term()}.
set_customer_user(User = #user{id = UserId}, SystemId) ->
	case get_customer_by_system_id(SystemId) of
		{ok, Customer = #customer{users = Users}} ->
			NewUsers = lists:keydelete(UserId, 2, Users),
			set_customer(SystemId, Customer#customer{users = [User | NewUsers]});
		Error -> Error
	end.

-spec del_customer_user(customer_id(), user_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer_user(SysCustomerId, UserId) ->
	case get_customer_by_system_id(SysCustomerId) of
		{ok, Customer = #customer{users = Users}} ->
			NewUsers = delete_user(Users, UserId),
			set_customer(SysCustomerId, Customer#customer{users = NewUsers});
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec find_user([#user{}], string()) -> {ok, #user{}} | {error, no_record}.
find_user([], _Uname) ->
	{error, no_record};
find_user([User = #user{id = Uname} | _RestUsers], Uname) ->
	{ok, User};
find_user([_User | RestUsers], UserName) ->
	find_user(RestUsers, UserName).

delete_user(Users, UserId) ->
	lists:foldl(
		fun(CurrentUser = #user{id = CurrentUserId}, Acc)->
			case CurrentUserId of
				UserId -> Acc;
				_Any -> [CurrentUser | Acc]
			end
		end,
		[], Users).
