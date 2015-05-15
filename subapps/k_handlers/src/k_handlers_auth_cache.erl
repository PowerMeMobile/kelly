-module(k_handlers_auth_cache).

%% API
-export([
    start_link/0,
    set/2,
    get/1,
    clear/0
]).

%% Service API
-export([
    get_all/0
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

-include_lib("alley_common/include/gen_server_spec.hrl").

-type key()   :: term().
-type value() :: term().

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

-spec get(key()) -> {ok, value()} | {error, not_found}.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            {error, not_found};
        [{Key, Value}] ->
            {ok, Value}
    end.

-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%% ===================================================================
%% Service API
%% ===================================================================

-spec get_all() -> [{key(), value()}].
get_all() ->
    ets:tab2list(?MODULE).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {ok, #state{}}.

handle_call({set, Key, Value}, _From, State = #state{}) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {reply, ok, State};
handle_call(clear, _From, State = #state{}) ->
    true = ets:delete_all_objects(?MODULE),
    {reply, ok, State};
handle_call(Request, _From, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
    {stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
