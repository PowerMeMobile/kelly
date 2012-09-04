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

-record(customer_map, {
	index :: {customer_id(), user_id()},
	map = [] :: [] | [{content_type(), subscription_id()}],
	wait_flag = false :: boolean() %% dedicate flag for every content type?
}).

%% ===================================================================
%% API Functions
%% ===================================================================

-export([
	start_link/0,
	register_subscription/1,
	unregister_subscription/3,
	get_subscription/3
]).

%% ===================================================================
%% GenServer Functions Exports
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

-spec unregister_subscription(CustomerID :: customer_id(), UserID :: user_id(),
	SubscriptionID :: subscription_id()) -> ok.
unregister_subscription(CustomerID, UserID, SubscriptionID) ->
	gen_server:cast(?MODULE, {unreg_sub, CustomerID, UserID, SubscriptionID}).

-spec get_subscription(CustomerID :: customer_id(), UserID :: user_id(),
	ContentType :: content_type()) ->
		{ok, SubscriptionID :: subscription_id()} |
		{error, no_subscription}.
get_subscription(CustomerID, UserID, ContentType) ->
	gen_server:call(?MODULE, {get_sub, CustomerID, UserID, ContentType}, infinity).

%% ===================================================================
%% GenServer Function Definitions
%% ===================================================================

init([]) ->
	TableID = ets:new(customers_map, [set, {keypos, #customer_map.index}]),
	{ok, Customers} = k_mb_db:get_customers(),
	lists:foreach(fun({CustomerID, UserID})->
			build_map(TableID, CustomerID, UserID)
		end, Customers),
    {ok, #state{storage = TableID}}.

handle_call({get_sub, CustomerID, UserID, ContentType}, _From,
	State = #state{storage = TableID}) ->
	Customer = get_customer(TableID, CustomerID, UserID),
	Reply = get_subscription(ContentType, Customer#customer_map.map),
	%% ?log_debug("ContentType: ~p, customer.map: ~p", [ContentType, Customer#customer_map.map]),
	set_flag(Reply, Customer, TableID),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({reg_sub, Subscription}, State = #state{storage = TableID}) ->

	#k_mb_subscription{
		customer_id = CustomerID,
		user_id = UserID
		} = Subscription,

	Customer = get_customer(TableID, CustomerID, UserID),
	build_map(TableID, CustomerID, UserID),
	process_postponed_items(Customer#customer_map.wait_flag, CustomerID, UserID),

	{noreply, State};

handle_cast({unreg_sub, CustomerID, UserID, SubscriptionID},
	State = #state{storage = TableID}) ->

	Customer = get_customer(TableID, CustomerID, UserID),
	?log_debug("customer: ~p", [Customer]),

	case lists:keymember(SubscriptionID, 2, Customer#customer_map.map) of
		true ->
			?log_debug("true", []),
			build_map(TableID, CustomerID, UserID);
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
%% Local Functions Definitions
%% ===================================================================

process_postponed_items(true, CustomerID, UserID) ->
	{ok, Items} = k_mb_db:get_wait(CustomerID, UserID),
	ChangeState = fun(ItemID) ->
		k_mb_db:set_pending(ItemID)
		end,
	[ChangeState(Item#k_mb_pending_item.item_id) || Item <- Items],
	Process = fun(Item) ->
		k_mb_wpool:process_incoming_item(Item)
		end,
	lists:foreach(Process, Items);
process_postponed_items(false, _CustomerID, _UserID) ->
	ok.

get_customer(TableID, CustomerID, UserID) ->
		case ets:lookup(TableID, {CustomerID, UserID}) of
			[Customer = #customer_map{}] -> Customer;
			[] -> #customer_map{index = {CustomerID, UserID}, map = []}
		end.

get_subscription(ContentType, Map) ->
	case proplists:get_value(ContentType, Map) of
		undefined -> {error, no_subscription};
		SubID -> {ok, SubID}
	end.

set_flag({ok, _SubID}, _Customer, _TableID) ->
	ok;
set_flag(_Reply, Customer, TableID) ->
	ets:insert(TableID, Customer#customer_map{wait_flag = true}).

%% ==== Build map functions ====

content_types() -> [
		<<"OutgoingBatch">>,
		<<"ReceiptBatch">>
	].

build_map(TableID, CustomerID, UserID) ->
	{ok, Subscriptions} = k_mb_db:get_subscriptions(CustomerID, UserID),
	{ok, Map} = build_map(Subscriptions),
	ets:insert(TableID, #customer_map{index = {CustomerID, UserID}, map = Map}).


build_map(Subscriptions) ->
	build_map_(content_types(), Subscriptions, []).

build_map_([], _, Map) -> {ok, Map};
build_map_([ContentType | Rest], Subscriptions, Map) ->
	NMap =
	case get_suitable_subscription(ContentType, Subscriptions, []) of
		undefined ->
			Map;
		SubscriptionID ->
			[{ContentType, SubscriptionID} | Map]
	end,
	build_map_(Rest, Subscriptions, NMap).

get_suitable_subscription(_ContentType, [], []) ->
	undefined;
get_suitable_subscription(_ContentType, [], [Sub | SubSoFar]) ->
	% priority resolving
	MaxFun = fun(NextSub, CurrentSub)->
		if
			NextSub#k_mb_subscription.priority > CurrentSub#k_mb_subscription.priority -> NextSub;
			true -> CurrentSub
		end
	end,
	Subscription = lists:foldl(MaxFun, Sub, SubSoFar),
	Subscription#k_mb_subscription.id;
get_suitable_subscription(ContentType, [Sub | SubSoFar], Acc) ->
	AppType = Sub#k_mb_subscription.app_type,
	SubType = Sub#k_mb_subscription.type,
	NewAcc =
		case can_handle({AppType, SubType}, ContentType) of
			true -> [Sub] ++ Acc;
			false -> Acc
		end,
	get_suitable_subscription(ContentType, SubSoFar, NewAcc).


can_handle({oneapi, dlvrReceiptReceiver}, ContentType)
	when (
			ContentType == <<"ReceiptBatch">>
	) -> true;
can_handle({oneapi, incomingSMSReceiver}, ContentType)
	when (
			ContentType == <<"OutgoingBatch">>
	)-> true;
can_handle({smpp,transceiver}, ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle({smpp,receiver}, ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle(_ConnectionType, _ContentType) -> false.
