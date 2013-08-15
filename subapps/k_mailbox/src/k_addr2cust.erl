-module(k_addr2cust).

-export([
	link/3,
	unlink/1,
	resolve/1,
	available_addresses/2
]).

-include("application.hrl").
-include("address.hrl").

-spec resolve(addr()) -> {ok, customer_id(), user_id()} |
						 {error, addr_not_used}.
resolve(Address = #addr{}) ->
	Selector = {
		'address' , k_storage_utils:addr_to_doc(Address)
	},
	case mongodb_storage:find(static_storage, ?msisdnsColl, Selector) of
		{ok, []} -> {error, addr_not_used};
		{ok, [{_, Doc}]} ->
			CustID = bsondoc:at(customer_id, Doc),
			UserID = bsondoc:at(user_id, Doc),
			{ok, CustID, UserID}
	end.

-spec link(addr(), customer_id(), user_id()) -> ok | {error, addr_in_use}.
link(Address = #addr{}, CustID, UserID) ->
	Modifier = {
		'address'     , k_storage_utils:addr_to_doc(Address),
		'customer_id' , CustID,
		'user_id'     , UserID
	},
	case resolve(Address) of
		{error, addr_not_used} ->
			{ok, _ID} = mongodb_storage:insert(static_storage, ?msisdnsColl, Modifier),
			ok;
		_ -> {error, addr_in_use}
	end.

-spec unlink(addr()) -> ok | {error, addr_not_used}.
unlink(Address = #addr{}) ->
	Selector = {
		'address' , k_storage_utils:addr_to_doc(Address)
	},
	ok = mongodb_storage:delete(static_storage, ?msisdnsColl, Selector).

-spec available_addresses(customer_id(), user_id()) ->
	{ok, [addr()]}.
available_addresses(CustID, UserID) ->
	Selector = {
		'customer_id' , CustID,
		'user_id'     , UserID
	},
	{ok, Docs} = mongodb_storage:find(static_storage, ?msisdnsColl, Selector),
	AddrDocs = [bsondoc:at(address, Doc) || {_, Doc} <- Docs],
	Items = [k_storage_utils:doc_to_addr(Doc) || Doc <- AddrDocs],
	{ok, Items}.
