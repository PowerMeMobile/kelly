%% @TODO convert subscription key
%% SubscriptionID -> {CustomerUuid, UserId, SubId}
%% to protect subscriptions of other users

-module(k_mb_subscription_mgr).

-behaviour(gen_server).

-include_lib("k_storage/include/mailbox.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").

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
    key                 :: {customer_uuid(), user_id()},
    subscriptions = []  :: [k_mb_subscription()]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register(k_mb_subscription()) -> ok.
register(Sub) ->
    ok = k_storage_mailbox:save_sub(Sub),
    gen_server:cast(?MODULE, {reg_sub, Sub}).

-spec unregister(uuid(), customer_uuid(), user_id()) -> ok.
unregister(SubId, CustomerUuid, UserId) ->
    ok = k_storage_mailbox:delete_subscription(SubId),
    gen_server:cast(?MODULE, {unreg_sub, SubId, CustomerUuid, UserId}).

-spec get_suitable_subscription(k_mb_item()) ->
    {ok, k_mb_subscription()} | undefined.
get_suitable_subscription(Item = #k_mb_oneapi_receipt{}) ->
    case k_storage_mailbox:get_subscription_for_oneapi_receipt(Item) of
        undefined ->
            process_get_suitable_sub_req(Item);
        {ok, Sub = #k_mb_oneapi_receipt_sub{}} ->
            {ok, Sub}
    end;
get_suitable_subscription(Item) ->
    process_get_suitable_sub_req(Item).

-spec process_funnel_down_event() -> ok.
process_funnel_down_event() ->
    {ok, Subs} = k_storage_mailbox:get_funnel_subscriptions(),
    Fun = fun(Sub) ->
        ?MODULE:unregister(
            Sub#k_mb_funnel_sub.id,
            Sub#k_mb_funnel_sub.customer_uuid,
            Sub#k_mb_funnel_sub.user_id
        )
    end,
    lists:foreach(Fun, Subs).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, {keypos, #subscriptions.key}]),
    {ok, SubIds} = k_storage_mailbox:get_subscription_ids(),
    Insert = fun(SubId) ->
        {ok, Sub} = k_storage_mailbox:get_subscription(SubId),
        ets_insert_sub(Sub)
    end,
    lists:foreach(Insert, SubIds),
    {ok, #state{}}.

handle_call({get_subscriptions, Key}, _From, State = #state{}) ->
    {ok, Subs} = ets_lookup_subs(Key), %% Key::{customer_uuid(), user_id()}
    {reply, {ok, Subs}, State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({reg_sub, Sub}, State = #state{}) ->
    ets_insert_sub(Sub),
    process_pending_items(Sub),
    {noreply, State};

handle_cast({unreg_sub, SubId, CustomerUuid, UserId}, State = #state{}) ->
    Key = {CustomerUuid, UserId},
    {ok, CurrentUserSubs} = ets_lookup_subs(Key),
    NewUserSubs = lists:keydelete(SubId, 2, CurrentUserSubs),
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
    {CustomerUuid, UserId} = get_customer_user(Sub),
    {ok, Receipts} = k_storage_mailbox:get_funnel_receipt_ids(CustomerUuid, UserId),
    [k_mb_wpool:process_incoming_item(Item) || Item <- Receipts],
    {ok, Incomings} = k_storage_mailbox:get_incoming_ids(CustomerUuid, UserId),
    [k_mb_wpool:process_incoming_item(Item) || Item <- Incomings];
process_pending_items(Sub = #k_mb_oneapi_receipt_sub{}) ->
    {CustomerUuid, UserId} = get_customer_user(Sub),
    {ok, Receipts} = k_storage_mailbox:get_oneapi_receipt_ids(CustomerUuid, UserId),
    [k_mb_wpool:process_incoming_item(Item) || Item <- Receipts];
process_pending_items(Sub = #k_mb_oneapi_incoming_sms_sub{}) ->
    {CustomerUuid, UserId} = get_customer_user(Sub),
    {ok, Incomings} = k_storage_mailbox:get_incoming_ids(CustomerUuid, UserId),
    [k_mb_wpool:process_incoming_item(Item) || Item <- Incomings].

process_get_suitable_sub_req(Item) ->
    Key = get_customer_user(Item),
    {ok, Subs} = gen_server:call(?MODULE, {get_subscriptions, Key}),
    get_suitable_subscription(Item, Subs).

get_suitable_subscription(#k_mb_funnel_receipt{}, Subs) ->
    case lists:keysearch(k_mb_funnel_sub, 1, Subs) of
        {value, Sub} -> {ok, Sub};
        false -> undefined
    end;
get_suitable_subscription(Item = #k_mb_incoming_sms{}, Subs) ->
    SuitableSubs = [Sub || Sub <- Subs, is_suitable_for_incoming_sms(Item, Sub)],
    resolve_subscription_priority(SuitableSubs);
get_suitable_subscription(#k_mb_oneapi_receipt{src_addr = SrcAddr}, Subs) ->
    ?log_debug("Subscriptions: ~p", [Subs]),
    ?log_debug("SrcAddr: ~p", [SrcAddr]),
    case lists:keyfind(SrcAddr, #k_mb_oneapi_receipt_sub.src_addr, Subs) of
        false ->
            undefined;
        Sub ->
            {ok, Sub}
    end;
get_suitable_subscription(_, _) ->
    undefined.

is_suitable_for_incoming_sms(_Item, #k_mb_funnel_sub{}) ->
    true;
is_suitable_for_incoming_sms(Item, Sub = #k_mb_oneapi_incoming_sms_sub{}) when
    Sub#k_mb_oneapi_incoming_sms_sub.dst_addr =:= Item#k_mb_incoming_sms.dst_addr
    andalso Sub#k_mb_oneapi_incoming_sms_sub.criteria =:= undefined ->
    true;
is_suitable_for_incoming_sms(Item, Sub = #k_mb_oneapi_incoming_sms_sub{}) when
    Sub#k_mb_oneapi_incoming_sms_sub.dst_addr =:= Item#k_mb_incoming_sms.dst_addr ->
    Criteria = Sub#k_mb_oneapi_incoming_sms_sub.criteria,
    Body = Item#k_mb_incoming_sms.body,
    case bstr:prefix(Body, Criteria) of
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

priority(Sub = #k_mb_oneapi_incoming_sms_sub{}) ->
    Sub#k_mb_oneapi_incoming_sms_sub.priority;
priority(Sub = #k_mb_funnel_sub{}) ->
    Sub#k_mb_funnel_sub.priority.

ets_insert_sub(Sub) ->
    Key = get_customer_user(Sub),
    {ok, CurrentUserSubs} = ets_lookup_subs(Key),
    {ok, NewSubs} = join_subscriptions(Sub, CurrentUserSubs),
    ets_insert(Key, NewSubs).

join_subscriptions(NewSub = #k_mb_funnel_sub{}, CurrentUserSubs) ->
    NewSubs =
    case lists:keytake(k_mb_funnel_sub, 1, CurrentUserSubs) of
        {value, ExpiredFunnelSub, PurgedSubs} ->
            ok = k_storage_mailbox:delete_subscription(ExpiredFunnelSub#k_mb_funnel_sub.id),
            [NewSub] ++ PurgedSubs;
        false ->
            [NewSub] ++ CurrentUserSubs
    end,
    {ok, NewSubs};
join_subscriptions(NewSub, CurrentUserSubs) ->
    NewSubs = [NewSub] ++ CurrentUserSubs,
    {ok, NewSubs}.

ets_insert(Key, Subs) ->
    true = ets:insert(?MODULE, #subscriptions{key = Key, subscriptions = Subs}).

ets_lookup_subs(Key) ->
    Subscriptions =
        case ets:lookup(?MODULE, Key) of
            [Subs = #subscriptions{}] -> Subs#subscriptions.subscriptions;
            [] -> []
        end,
    {ok, Subscriptions}.

get_customer_user(Sub = #k_mb_oneapi_receipt_sub{}) ->
    CustomerUuid = Sub#k_mb_oneapi_receipt_sub.customer_uuid,
    UserId = Sub#k_mb_oneapi_receipt_sub.user_id,
    {CustomerUuid, UserId};
get_customer_user(Sub = #k_mb_oneapi_incoming_sms_sub{}) ->
    CustomerUuid = Sub#k_mb_oneapi_incoming_sms_sub.customer_uuid,
    UserId = Sub#k_mb_oneapi_incoming_sms_sub.user_id,
    {CustomerUuid, UserId};
get_customer_user(Sub = #k_mb_funnel_sub{}) ->
    CustomerUuid = Sub#k_mb_funnel_sub.customer_uuid,
    UserId = Sub#k_mb_funnel_sub.user_id,
    {CustomerUuid, UserId};
get_customer_user(Item = #k_mb_oneapi_receipt{}) ->
    CustomerUuid = Item#k_mb_oneapi_receipt.customer_uuid,
    UserId = Item#k_mb_oneapi_receipt.user_id,
    {CustomerUuid, UserId};
get_customer_user(Item = #k_mb_funnel_receipt{}) ->
    CustomerUuid = Item#k_mb_funnel_receipt.customer_uuid,
    UserId = Item#k_mb_funnel_receipt.user_id,
    {CustomerUuid, UserId};
get_customer_user(Item = #k_mb_incoming_sms{}) ->
    CustomerUuid = Item#k_mb_incoming_sms.customer_uuid,
    UserId = Item#k_mb_incoming_sms.user_id,
    {CustomerUuid, UserId}.
