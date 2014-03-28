%% @TODO convert subscription key
%% SubscriptionID -> {CustomerID, UserID, SubscriptionID}
%% to protect subscriptions of other users

-module(k_mb_subscription_mgr).

-behaviour(gen_server).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include("application.hrl").

%% API
-export([
	start_link/0,
	register/1,
	unregister/3,
	get_suitable_subscription/1,
	process_funnel_down_event/0
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
    terminate/2,
	code_change/3
]).

-record(state, {
}).

-record(subscriptions, {
	key 				:: {customer_id(), user_id()},
	subscriptions = [] 	:: [k_mb_subscription()]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register(Subscription::k_mb_subscription()) -> ok.
register(Subscription) ->
	ok = k_mb_db:save_sub(Subscription),
	gen_server:cast(?MODULE, {reg_sub, Subscription}).

-spec unregister(
	SubscriptionID::binary(),
    CustomerID::customer_id(),
    UserID::user_id()
) -> ok.
unregister(SubscriptionID, CustomerID, UserID) ->
	ok = k_mb_db:delete_subscription(SubscriptionID),
	gen_server:cast(?MODULE, {unreg_sub, SubscriptionID, CustomerID, UserID}).

-spec get_suitable_subscription(k_mb_item()) ->
	{ok, k_mb_subscription()} | undefined.
get_suitable_subscription(Item = #k_mb_k1api_receipt{}) ->
	case k_mb_db:get_subscription_for_k1api_receipt(Item) of
		undefined ->
			process_get_suitable_sub_req(Item);
		{ok, Subscription = #k_mb_k1api_receipt_sub{}} ->
			{ok, Subscription}
	end;
get_suitable_subscription(Item) ->
	process_get_suitable_sub_req(Item).

-spec process_funnel_down_event() -> ok.
process_funnel_down_event() ->
	{ok, Subs} = k_mb_db:get_funnel_subscriptions(),
	Fun = fun(Sub) ->
		?MODULE:unregister(
			Sub#k_mb_funnel_sub.id,
			Sub#k_mb_funnel_sub.customer_id,
			Sub#k_mb_funnel_sub.user_id
		)
	end,
	lists:foreach(Fun, Subs).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?MODULE = ets:new(?MODULE, [set, named_table, {keypos, #subscriptions.key}]),
	{ok, SubscriptionIDs} = k_mb_db:get_subscription_ids(),
	Insert = fun(SubscriptionID) ->
		{ok, Subscription} = k_mb_db:get_subscription(SubscriptionID),
		ets_insert_sub(Subscription)
	end,
	lists:foreach(Insert, SubscriptionIDs),
	{ok, #state{}}.

handle_call({get_subscriptions, Key}, _From, State = #state{}) ->
	{ok, Subscriptions} = ets_lookup_subs(Key), %% Key::{customer_id(), user_id()}
	{reply, {ok, Subscriptions}, State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({reg_sub, Subscription}, State = #state{}) ->
	ets_insert_sub(Subscription),
	process_pending_items(Subscription),
	{noreply, State};

handle_cast({unreg_sub, SubscriptionID, CustomerID, UserID}, State = #state{}) ->
	Key = {CustomerID, UserID},
	{ok, CurrentUserSubs} = ets_lookup_subs(Key),
	NewUserSubs = lists:keydelete(SubscriptionID, 2, CurrentUserSubs),
	ets_insert(Key, NewUserSubs),
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
%% Internal
%% ===================================================================

process_pending_items(Sub = #k_mb_funnel_sub{}) ->
	{CustomerID, UserID} = get_customer_user(Sub),
	{ok, Items} = k_mb_db:get_funnel_receipts(CustomerID, UserID),
	[k_mb_wpool:process_incoming_item(Item) || Item <- Items];
process_pending_items(Subscription) ->
	{CustomerID, UserID} = get_customer_user(Subscription),
	{ok, ItemIDs} = k_mb_db:get_pending(CustomerID, UserID),
	lists:foreach(fun k_mb_wpool:process_incoming_item/1, ItemIDs).

process_get_suitable_sub_req(Item) ->
	Key = get_customer_user(Item), %% Key::{customer_id(), user_id()}
	{ok, Subscriptions} = gen_server:call(?MODULE, {get_subscriptions, Key}),
	get_suitable_subscription(Item, Subscriptions).

get_suitable_subscription(#k_mb_funnel_receipt{}, Subscriptions) ->
	case lists:keysearch(k_mb_funnel_sub, 1, Subscriptions) of
		{value, Subscription} -> {ok, Subscription};
		false -> undefined
	end;
get_suitable_subscription(Item = #k_mb_incoming_sms{}, Subscriptions) ->
	SuitableSubs = [Sub || Sub <- Subscriptions, is_suitable_for_incoming_sms(Item, Sub)],
	resolve_subscription_priority(SuitableSubs);
get_suitable_subscription(#k_mb_k1api_receipt{source_addr = SourceAddr}, Subscriptions) ->
	?log_debug("Subscriptions: ~p", [Subscriptions]),
	?log_debug("SourceAddr: ~p", [SourceAddr]),
	Fun =
    	fun(#k_mb_k1api_receipt_sub{dest_addr = Addr}) when Addr =:= SourceAddr ->
	    		true;
		    (_) ->
			    false
    	end,
	case ac_lists:findwith(Fun, Subscriptions) of
		false -> undefined;
		{value, Subscription} -> {ok, Subscription}
	end;
get_suitable_subscription(_, _) ->
	undefined.

is_suitable_for_incoming_sms(_Item, #k_mb_funnel_sub{}) ->
	true;
is_suitable_for_incoming_sms(Item, Sub = #k_mb_k1api_incoming_sms_sub{}) when
	Sub#k_mb_k1api_incoming_sms_sub.dest_addr =:= Item#k_mb_incoming_sms.dest_addr
	andalso Sub#k_mb_k1api_incoming_sms_sub.criteria =:= undefined ->
	true;
is_suitable_for_incoming_sms(Item, Sub = #k_mb_k1api_incoming_sms_sub{}) when
	Sub#k_mb_k1api_incoming_sms_sub.dest_addr =:= Item#k_mb_incoming_sms.dest_addr ->
	Criteria = Sub#k_mb_k1api_incoming_sms_sub.criteria,
	MessageBody = Item#k_mb_incoming_sms.message_body,
	case bstr:prefix(MessageBody, Criteria) of
		true -> true;
		false -> false
	end;
is_suitable_for_incoming_sms(_, _) ->
	false.

resolve_subscription_priority([]) ->
	undefined;
resolve_subscription_priority([Subscription]) ->
	{ok, Subscription};
resolve_subscription_priority([Subscription | RestSubs]) ->
	MaxFun = fun(NextSub, CurrentSub)->
		case priority(NextSub) > priority(CurrentSub) of
			true -> NextSub;
			false -> CurrentSub
		end
	end,
	Subscription = lists:foldl(MaxFun, Subscription, RestSubs),
	{ok, Subscription}.

priority(Sub = #k_mb_k1api_incoming_sms_sub{}) ->
	Sub#k_mb_k1api_incoming_sms_sub.priority;
priority(Sub = #k_mb_funnel_sub{}) ->
	Sub#k_mb_funnel_sub.priority.


ets_insert_sub(Subscription) ->
	Key = get_customer_user(Subscription), %% Key::{customer_id(), user_id()}
	{ok, CurrentUserSubs} = ets_lookup_subs(Key),
	{ok, NewSubs} = join_subscriptions(Subscription, CurrentUserSubs),
	ets_insert(Key, NewSubs).

join_subscriptions(NewSub = #k_mb_funnel_sub{}, CurrentUserSubs) ->
	NewSubs =
	case lists:keytake(k_mb_funnel_sub, 1, CurrentUserSubs) of
		{value, ExpiredFunnelSub, PurgedSubs} ->
			ok = k_mb_db:delete_subscription(ExpiredFunnelSub#k_mb_funnel_sub.id),
			[NewSub] ++ PurgedSubs;
		false ->
			[NewSub] ++ CurrentUserSubs
	end,
	{ok, NewSubs};
join_subscriptions(NewSub, CurrentUserSubs) ->
	NewSubs = [NewSub] ++ CurrentUserSubs,
	{ok, NewSubs}.

ets_insert(Key, Subscriptions) ->
	true = ets:insert(?MODULE, #subscriptions{key = Key, subscriptions = Subscriptions}).

ets_lookup_subs(Key) ->
	Subscriptions =
	case ets:lookup(?MODULE, Key) of
		[Subs = #subscriptions{}] -> Subs#subscriptions.subscriptions;
		[] -> []
	end,
	{ok, Subscriptions}.

get_customer_user(Sub = #k_mb_k1api_receipt_sub{}) ->
	CustomerID = Sub#k_mb_k1api_receipt_sub.customer_id,
	UserID = Sub#k_mb_k1api_receipt_sub.user_id,
	{CustomerID, UserID};
get_customer_user(Sub = #k_mb_k1api_incoming_sms_sub{}) ->
	CustomerID = Sub#k_mb_k1api_incoming_sms_sub.customer_id,
	UserID = Sub#k_mb_k1api_incoming_sms_sub.user_id,
	{CustomerID, UserID};
get_customer_user(Sub = #k_mb_funnel_sub{}) ->
	CustomerID = Sub#k_mb_funnel_sub.customer_id,
	UserID = Sub#k_mb_funnel_sub.user_id,
	{CustomerID, UserID};
get_customer_user(Item = #k_mb_k1api_receipt{}) ->
	CustomerID = Item#k_mb_k1api_receipt.customer_id,
	UserID = Item#k_mb_k1api_receipt.user_id,
	{CustomerID, UserID};
get_customer_user(Item = #k_mb_funnel_receipt{}) ->
	CustomerID = Item#k_mb_funnel_receipt.customer_id,
	UserID = Item#k_mb_funnel_receipt.user_id,
	{CustomerID, UserID};
get_customer_user(Item = #k_mb_incoming_sms{}) ->
	CustomerID = Item#k_mb_incoming_sms.customer_id,
	UserID = Item#k_mb_incoming_sms.user_id,
	{CustomerID, UserID}.
