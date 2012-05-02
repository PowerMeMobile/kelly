-module(k_mailbox).

-export([
	get_customer_worker/1,
	register_connection/3,
	unregister_connection/2,
	register_incoming_item/4
]).

-include("pending_item.hrl").

-spec get_customer_worker(string()) -> pid().
get_customer_worker(CustomerID) ->
	spawn(fun() ->
		supervisor:start_child(k_mailbox_worker_sup, [CustomerID])
	end),
	{Srv, _} = gproc:await({n, l, {k_mailbox_worker_srv, CustomerID}}),
	Srv.

-spec register_connection(CustomerID :: string(), ConnID :: string(), ConnType :: atom()) -> ok.
register_connection(CustID, ConnID, ConnType) ->
	Worker = get_customer_worker(CustID),
	gen_server:call(Worker, {register_connection, ConnID, ConnType}, infinity).

-spec unregister_connection(CustomerID :: string(), ConnID :: string()) -> ok.
unregister_connection(CustID, ConnID) ->
	Worker = get_customer_worker(CustID),
	gen_server:call(Worker, {unregister_connection, ConnID}, infinity).

-spec register_incoming_item(ItemID :: string(), CustomerID :: string(), ContentType :: binary() , ContentBody :: binary()) -> ok.
register_incoming_item(ItemID, CustID, ContentType, ContentBody) ->
	Item = #k_mb_pending_item{
		item_id = ItemID,
		customer_id = CustID,
		content_type = ContentType,
		content_body = ContentBody
	},
	{atomic, ok} = mnesia:transaction(fun() ->
		ok = mnesia:write(Item)
	end),
	Worker = get_customer_worker(CustID),
	gen_server:cast(Worker, {submit_item, Item}).
