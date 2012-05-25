%% @private

-module(k_mb_map_mgr).

-behaviour(gen_server).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include("subscription.hrl").
-include("pending_item.hrl").

-record(state, {
	storage :: term() %% tid()
}).

-type customer_id() :: string().
-type content_type() :: binary().
-type subscription_id() :: string().

-record(customer, {
	index :: customer_id(),
	map = [] :: [] | [{content_type(), subscription_id()}],
	wait_flag = false :: boolean() %% dedicate flag for every content type?
}).

%% ===================================================================
%% API Functions
%% ===================================================================

-export([
	start_link/0,
	register_subscription/1,
	unregister_subscription/2,
	get_subscription/2
]).

%% ===================================================================
%% gen_server Function Exports
%% ===================================================================

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
    terminate/2,
	code_change/3
]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_subscription(Subscription :: #k_mb_subscription{}) -> ok.
register_subscription(Subscription) ->
	gen_server:cast(?MODULE, {reg_sub, Subscription}).

-spec unregister_subscription(CustomerID :: customer_id(),
	SubscriptionID :: subscription_id()) -> ok.
unregister_subscription(CustomerID, SubscriptionID) ->
	gen_server:cast(?MODULE, {unreg_sub, CustomerID, SubscriptionID}).

-spec get_subscription(CustomerID :: string(),
	ContentType :: binary()) ->
		{ok, SubscriptionID :: string()} |
		{error, no_subscription}.
get_subscription(CustomerID, ContentType) ->
	gen_server:call(?MODULE, {get_sub, CustomerID, ContentType}).

%% ===================================================================
%% gen_server Function Definitions
%% ===================================================================

init([]) ->
	TableID = ets:new(customers, [set, {keypos, #customer.index}, private]),
	{ok, Customers} = k_mb_db:get_customers(),
	lists:foreach(fun(CustomerID)->
			build_map(TableID, CustomerID)
		end, Customers),
    {ok, #state{storage = TableID}}.

handle_call({get_sub, CustomerID, ContentType}, _From,
	State = #state{storage = TableID}) ->
	Customer = get_customer(TableID, CustomerID),
	Reply = search_subscription(ContentType, Customer#customer.map),
	?log_debug("ContentType: ~p, customer.map: ~p", [ContentType, Customer#customer.map]),
	set_flag(Reply, Customer, TableID),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({reg_sub, Subscription}, State = #state{storage = TableID}) ->

	#k_mb_subscription{
		customer_id = CustomerID
		} = Subscription,

	Customer = get_customer(TableID, CustomerID),
	build_map(TableID, CustomerID),
	process_postponed_items(Customer#customer.wait_flag, CustomerID),

	{noreply, State};

handle_cast({unreg_sub, CustomerID, SubscriptionID},
	State = #state{storage = TableID}) ->

	Customer = get_customer(TableID, CustomerID),
	?log_debug("customer: ~p", [Customer]),

	case lists:keymember(SubscriptionID, 2, Customer#customer.map) of
		true ->
			?log_debug("true", []),
			build_map(TableID, CustomerID);
		false ->
			?log_debug("false", []),
			ok
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

process_postponed_items(true, CustomerID) ->
	{ok, Items} = k_mb_db:get_wait(CustomerID),
	ChangeState = fun(ItemID) ->
		k_mb_db:set_pending(ItemID)
		end,
	[ChangeState(Item#k_mb_pending_item.item_id) || Item <- Items],
	Process = fun(Item) ->
		k_mb_wpool:process_incoming_item(Item)
		end,
	lists:foreach(Process, Items);
process_postponed_items(false, _CustomerID) ->
	ok.

get_customer(TableID, CustomerID) ->
		case ets:lookup(TableID, CustomerID) of
			[Customer = #customer{}] -> Customer;
			[] -> #customer{index = CustomerID, map = []}
		end.

search_subscription(ContentType, Map) ->
	case proplists:get_value(ContentType, Map) of
		undefined -> {error, no_subscription};
		SubID -> {ok, SubID}
	end.

set_flag({ok, _SubID}, _Customer, _TableID) ->
	ok;
set_flag(_Reply, Customer, TableID) ->
	ets:insert(TableID, Customer#customer{wait_flag = true}).

%% ==== Build map functions ====

content_types() -> [
		<<"OutgoingBatch">>,
		<<"ReceiptBatch">>
	].

build_map(TableID, CustomerID) ->
	{ok, Subscriptions} = k_mb_db:get_subscriptions(CustomerID),
	{ok, Map} = build_map(Subscriptions),
	ets:insert(TableID, #customer{index = CustomerID, map = Map}).


build_map(Subscriptions) ->
	build_map_(content_types(), Subscriptions, []).

build_map_([], _, Map) -> {ok, Map};
build_map_([ContentType | ContentTypesSoFar], Subscriptions, Map) ->
	NMap = case get_suitable_subscription(ContentType, Subscriptions) of
		undefined ->
			Map;
		SubID ->
			[{ContentType, SubID} | Map]
	end,
	build_map_(ContentTypesSoFar, Subscriptions, NMap).


get_suitable_subscription(_ContentType, []) -> undefined;
get_suitable_subscription(ContentType, [Sub | SubsSoFar]) ->
	SubType = Sub#k_mb_subscription.sub_type,
	case can_handle(SubType, ContentType) of
		true -> Sub#k_mb_subscription.id;
		false -> get_suitable_subscription(ContentType, SubsSoFar)
	end.

can_handle('smpp.transceiver', ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle('smpp.receiver', ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle(_ConnectionType, _ContentType) -> false.