-module(k_addr2cust).

-export([
	link/2,
	unlink/1,
	resolve/1
]).

-include("address.hrl").

-spec link(
	Addr :: address(),
	CustID :: string()
) -> ok | {error, addr_in_use}.
-spec unlink(
	Addr :: address()
) -> ok | {error, addr_not_used}.


resolve(Addr) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Addr}) of
			[] -> {error, addr_not_used};
			[#k_mb_address{customer_id = CustID}] -> {ok, CustID}
		end
	end),
	Result.

link(Addr, CustID) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Addr}) of
			[] ->
				ok = mnesia:write(#k_mb_address{
					address = Addr,
					customer_id = CustID
				});
			_ -> {error, addr_in_use}
		end
	end),
	Result.

unlink(Addr) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Addr}) of
			[] ->
				{error, addr_not_used};
			_ ->
				mnesia:delete({k_mb_address, Addr}),
				ok
		end
	end),
	Result.
