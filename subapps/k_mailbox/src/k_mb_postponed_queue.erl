-module(k_mb_postponed_queue).

-behaviour(gen_server).

-include("application.hrl").
-include_lib("k_storage/include/mailbox.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").

-record(state, {
    rbuff :: term(),
    timeout :: integer()
}).


%% API
-export([
    start_link/0,
    postpone/1
]).


%% GenServer Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec postpone(term()) -> {postponed, Seconds::integer()} | {error, reached_max}.
postpone(Item) ->
    {ok, CurrentAttempt} = get_current_attempt(Item),
    MaxRetry = k_mb_config:get_env(max_retry),
    postpone(Item, CurrentAttempt, MaxRetry).

%% ===================================================================
%% GenServer Functions Definitions
%% ===================================================================

init([]) ->
    MaxRetry = k_mb_config:get_env(max_retry),
    Size = position(MaxRetry),
    Default = [],
    RBuf = rbuf:new(Size, random, Default),
    Timeout = k_mb_config:get_env(repeat_delay),
    {ok, #state{rbuff = RBuf, timeout = Timeout}, Timeout}.

handle_call({postpone, Item}, _From, State = #state{rbuff = RBuf, timeout = T}) ->
    {ok, Attempt} = get_current_attempt(Item),
    Index = position(Attempt),
    RetryTime = T * Index / 1000,
    {{value, ItemList}, RBuf} = rbuf:get(Index, RBuf),
    NewList = [Item] ++ ItemList,
    {ok, NewRBuf} = rbuf:set(NewList, Index, RBuf),
    {reply, {postponed, RetryTime}, State#state{rbuff = NewRBuf}, T};

handle_call(_Request, _From, State) ->
    {stop, {bad_request}, State}.

handle_cast(_Msg, State) ->
    {stop, {bad_request}, State}.

handle_info(timeout, State = #state{rbuff = RBuf, timeout = T}) ->
    {{value, ItemList}, NewRBuf} = rbuf:get(RBuf),
    process(ItemList),
    {noreply, State#state{rbuff = NewRBuf}, T};
handle_info(_Info, State) ->
    {stop, {bad_request}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

process(ItemList) ->
    lists:foreach(fun k_mb_wpool:process_incoming_item/1, ItemList).

position(N) ->
    trunc(math:pow(2, N-1)).

postpone(_Item, CurrentAttempt, MaxRetry) when CurrentAttempt >= MaxRetry ->
    {error, reached_max};
postpone(Item, Attempt, _MaxRetry) ->
    gen_server:call(?MODULE, {postpone, increment_attempt(Item, Attempt)}).

get_current_attempt(Item = #k_mb_funnel_receipt{}) ->
    {ok, Item#k_mb_funnel_receipt.delivery_attempt};
get_current_attempt(Item = #k_mb_oneapi_receipt{}) ->
    {ok, Item#k_mb_oneapi_receipt.delivery_attempt};
get_current_attempt(Item = #k_mb_incoming_sms{}) ->
    {ok, Item#k_mb_incoming_sms.delivery_attempt}.

increment_attempt(Item = #k_mb_funnel_receipt{}, Attempt) ->
    Item#k_mb_funnel_receipt{delivery_attempt = Attempt + 1};
increment_attempt(Item = #k_mb_oneapi_receipt{}, Attempt) ->
    Item#k_mb_oneapi_receipt{delivery_attempt = Attempt + 1};
increment_attempt(Item = #k_mb_incoming_sms{}, Attempt) ->
    Item#k_mb_incoming_sms{delivery_attempt = Attempt + 1}.
