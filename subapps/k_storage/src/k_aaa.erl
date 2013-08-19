-module(k_aaa).

%% API
-export([
	set_customer/1,
	del_customer/1,
	get_customers/0,
	get_customer_by_uuid/1,
	get_customer_by_id/1,
	get_customer_user/2,
	set_customer_user/2,
	del_customer_user/2
]).

-include("customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(#customer{}) -> ok | {error, term()}.
set_customer(Customer = #customer{customer_uuid = CustomerUUID}) ->
	k_aaa_customers:set_customer(CustomerUUID, Customer).

-spec get_customers() -> {ok, [{customer_id(), #customer{}}]} | {error, term()}.
get_customers() ->
	k_aaa_customers:get_customers().

-spec get_customer_by_id(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_id(CustomerId) ->
	k_aaa_customers:get_customer_by_id(CustomerId).

-spec get_customer_by_uuid(customer_uuid()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer_by_uuid(CustomerUUID) ->
	k_aaa_customers:get_customer_by_uuid(CustomerUUID).

-spec del_customer(customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerUUID) ->
	k_aaa_customers:del_customer(CustomerUUID).

-spec get_customer_user(#customer{}, UserId :: string()) -> {ok, #user{}} | {error, no_entry} | {error, term()}.
get_customer_user(#customer{users = UserList}, UserId) ->
	find_user(UserList, UserId).

-spec set_customer_user(#user{}, customer_uuid()) -> ok | {error, no_entry} | {error, term()}.
set_customer_user(User = #user{id = UserId}, CustomerUUID) ->
	case get_customer_by_uuid(CustomerUUID) of
		{ok, Customer = #customer{users = Users}} ->
			NewUsers = lists:keydelete(UserId, #user.id, Users),
			k_aaa_customers:set_customer(CustomerUUID, Customer#customer{users = [User | NewUsers]});
		Error -> Error
	end.

-spec del_customer_user(customer_uuid(), user_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer_user(CustomerUUID, UserId) ->
	case get_customer_by_uuid(CustomerUUID) of
		{ok, Customer = #customer{users = Users}} ->
			NewUsers = delete_user(Users, UserId),
			k_aaa_customers:set_customer(CustomerUUID, Customer#customer{users = NewUsers});
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec find_user([#user{}], string()) -> {ok, #user{}} | {error, no_entry}.
find_user([], _Uname) ->
	{error, no_entry};
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
		end, [], Users
	).
