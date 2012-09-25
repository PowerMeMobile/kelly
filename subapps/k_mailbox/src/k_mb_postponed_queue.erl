%% @private

-module(k_mb_postponed_queue).

-behaviour(gen_server).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include("pending_item.hrl").

-record(state, {
    rbuff :: term(),
    timeout :: integer()
    }).

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	start_link/0,
    postpone/1
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
%% API Functions Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec postpone(Item :: #k_mb_pending_item{}) ->
	{postponed, Seconds :: integer()} |
	{error, rich_max}.
postpone(Item = #k_mb_pending_item{attempt = CurrentAttempt}) ->
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
    #k_mb_pending_item{
        attempt = Attempt
    } = Item,
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
%% Local Functions Definitions
%% ===================================================================

process(ItemList) ->
    lists:foreach(fun(Item) ->
        k_mb_wpool:process_incoming_item(Item)
        end, ItemList).

position(N) ->
    trunc(math:pow(2, N - 2)) + 1.

postpone(_Item, CurrentAttempt, MaxRetry) when CurrentAttempt == MaxRetry ->
	{error, rich_max};
    %% ?log_warn("Rich max retry attempts, removing item. Error: ~p",
	%% 	[Item#k_mb_pending_item.error]),
    %% k_mb_db:delete_items([Item#k_mb_pending_item.item_id]);
postpone(Item = #k_mb_pending_item{}, Attempt, _MaxRetry) ->
    gen_server:call(?MODULE, {postpone, Item#k_mb_pending_item{attempt = Attempt + 1}}).
