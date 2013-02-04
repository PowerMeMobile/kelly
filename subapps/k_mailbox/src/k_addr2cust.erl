-module(k_addr2cust).

-export([
	link/3,
	unlink/1,
	resolve/1,
	available_addresses/2
]).

-include_lib("stdlib/include/qlc.hrl").
-include("application.hrl").
-include("address.hrl").

-spec resolve(addr()) -> {ok, customer_id(), user_id()} |
						 {error, addr_not_used}.
resolve(Address = #addr{}) ->
	Selector = [{address, k_storage:addr_to_doc(Address)}],
	case mongodb_storage:find(?msisdnsColl, Selector) of
		{ok, []} -> {error, addr_not_used};
		{ok, [{_, Plist}]} ->
			CustID = proplists:get_value(customer_id, Plist),
			UserID = proplists:get_value(user_id, Plist),
			{ok, CustID, UserID}
	end.

-spec link(addr(), customer_id(), user_id()) -> ok | {error, addr_in_use}.
link(Address = #addr{}, CustID, UserID) ->
	Plist = [
		{address, k_storage:addr_to_doc(Address)},
		{customer_id, CustID},
		{user_id, UserID}
	],
	case resolve(Address) of
		{error, addr_not_used} ->
			io:format("Link addr: ~p~n", [Plist]),
			{ok, {_ID}} = mongodb_storage:insert(?msisdnsColl, Plist),
			ok;
		_ -> {error, addr_in_use}
	end.

-spec unlink(addr()) -> ok | {error, addr_not_used}.
unlink(Address = #addr{}) ->
	Plist = k_storage:addr_to_doc(Address),
	ok = mongodb_storage:delete(?msisdnsColl, [{address, Plist}]).

-spec available_addresses(customer_id(), user_id()) ->
	{ok, [addr()]}.
available_addresses(CustID, UserID) ->
	Selector = [
		{customer_id, CustID},
		{user_id, UserID}
	],
	{ok, Plists} = mongodb_storage:find(?msisdnsColl, Selector),
	AddrDocs = [proplists:get_value(address, Plist) || {_, Plist} <- Plists],
	Items = [k_storage:doc_to_addr(Doc) || Doc <- AddrDocs],
	{ok, Items}.
