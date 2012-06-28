-module(k_addr2cust).

-export([
	link/3,
	unlink/1,
	resolve/1,
	available_addresses/2
]).

-include_lib("stdlib/include/qlc.hrl").
-include("address.hrl").

-spec resolve(addr()) -> {ok, customer_id(), user_id()} |
						 {error, addr_not_used}.
resolve(Address = #addr{}) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Address}) of
			[] ->
				{error, addr_not_used};
			[#k_mb_address{customer_id = CustID, user_id = UserID}] ->
				{ok, CustID, UserID}
		end
	end),
	Result.

-spec link(addr(), customer_id(), user_id()) -> ok | {error, addr_in_use}.
link(Address = #addr{}, CustID, UserID) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Address}) of
			[] ->
				ok = mnesia:write(#k_mb_address{
					address = Address,
					customer_id = CustID,
					user_id = UserID
				});
			_ -> {error, addr_in_use}
		end
	end),
	Result.

-spec unlink(addr()) -> ok | {error, addr_not_used}.
unlink(Address = #addr{}) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Address}) of
			[] ->
				{error, addr_not_used};
			_ ->
				mnesia:delete({k_mb_address, Address}),
				ok
		end
	end),
	Result.

-spec available_addresses(customer_id(), user_id()) ->
	{ok, [addr()]}.
available_addresses(CustID, UserID) ->
	{atomic, Items} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item#k_mb_address.address || Item <- mnesia:table(k_mb_address),
				Item#k_mb_address.customer_id == CustID andalso
				Item#k_mb_address.user_id == UserID
		]))
	end),
	{ok, Items}.
