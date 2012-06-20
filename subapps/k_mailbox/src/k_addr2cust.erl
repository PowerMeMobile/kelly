-module(k_addr2cust).

-export([
	link/2,
	unlink/1,
	resolve/1,
	available_addresses/1
]).

-include_lib("k_common/include/storages.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("address.hrl").

-spec resolve(address()) -> {ok, CustID :: string()} | {error, addr_not_used}.
resolve({Addr, Ton, Npi}) ->
	Address = #addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
		},
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Address}) of
			[] -> {error, addr_not_used};
			[#k_mb_address{customer_id = CustID}] -> {ok, CustID}
		end
	end),
	Result.

-spec link(address(), CustID :: string()) -> ok | {error, addr_in_use}.
link({Addr, Ton, Npi}, CustID) ->
	Address = #addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
		},
	{atomic, Result} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_address, Address}) of
			[] ->
				ok = mnesia:write(#k_mb_address{
					address = Address,
					customer_id = CustID
				});
			_ -> {error, addr_in_use}
		end
	end),
	Result.

-spec unlink(address()) -> ok | {error, addr_not_used}.
unlink({Addr, Ton, Npi}) ->
	Address = #addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
		},
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

-spec available_addresses(CustID :: string()) ->
	{ok, [address()]}.
available_addresses(CustID) ->
	{atomic, Items} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				Item#k_mb_address.address || Item <- mnesia:table(k_mb_address),
				Item#k_mb_address.customer_id == CustID
		]))
	end),
	{ok, Items}.
