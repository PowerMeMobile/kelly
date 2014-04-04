-module(k_snmp_tasks_queue).

-behaviour(gen_server).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include("snmp_task.hrl").

%% API
-export([
    start_link/0,
    next/0,
    save/1,
    ack/1
]).

%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    table :: term(),
    next_task :: undefined | integer(),
    next_id :: integer()
}).

%%%
%%% API
%%%

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec next() -> {ok, #task{}}.
next() ->
    {ok, Task} = gen_server:call(?MODULE, give_task),
    {ok, Task}.

-spec save(#task{}) -> {ok, #task{}}.
save(Task) ->
    {ok, Task} = gen_server:call(?MODULE, {save_task, Task}).

-spec ack(Key :: integer()) -> {ok, integer()}.
ack(Key) ->
    {ok, Key} = gen_server:call(?MODULE, {ack_task, Key}).

%%%
%%% Callbacks
%%%

init(_Args) ->
    ?log_debug("init...", []),
    Name = ?MODULE,
    {ok, CWD} = file:get_cwd(),
    FileName = filename:join(CWD, "data/snmp_tasks_queue.dets"),
    Args = [
        {auto_save, 60000},
        {file, FileName},
        {type, set},
        {keypos, 2}
    ],
    {ok, Name} = dets:open_file(Name, Args),

    IdList = dets:select(Name,[{ #task{id = '$1',
                                        function = '_',
                                        args = '_'},
                        [],
                        ['$1']}]),
    ?log_debug("IdList: ~p ", [IdList]),
    case IdList of
        [] ->
            NextTask = undefined,
            NextId = 1,
            {ok, #state{table = Name, next_id = NextId, next_task = NextTask}};
        _Any ->
            NextTask = lists:min(IdList),
            NextId = lists:max(IdList) + 1,
            {ok, #state{table = Name, next_id = NextId, next_task = NextTask}}
    end.

%% in case of empty storage
handle_call({save_task, Task}, _From, State = #state{
                                        next_task = undefined,
                                        next_id = NextId}) ->
    true = dets:insert_new(?MODULE, Task#task{id = NextId}),
    {reply, {ok, Task}, State#state{next_id = NextId + 1, next_task = NextId}};

%% in case of NOT EMPTY storge
handle_call({save_task, Task}, _From, State = #state{
                                        next_id = NextId}) ->
    true = dets:insert_new(?MODULE, Task#task{id = NextId}),
    {reply, {ok, Task}, State#state{next_id = NextId + 1}};


%% in case of empty storage
handle_call(give_task, _From, State = #state{
                                    next_task = undefined}) ->

    {reply, {ok, []}, State};

%% in case of availability of NextTask
handle_call(give_task, _From, State = #state{
                                    next_task = NextTask}) ->
    [Task = #task{}] = dets:lookup(?MODULE, NextTask),
    {reply, {ok, Task}, State};

%% in case of correct Key
handle_call({ack_task, Key}, _From, State = #state{
                                    next_task = Key,
                                    next_id = NextId}) ->
    dets:delete(?MODULE, Key),
    NextTask = Key + 1,
    case NextTask of
        %% there are NOT any tasks in the storage
        NextId ->
            {reply, {ok, Key}, State#state{
                                    next_task = undefined,
                                    next_id = 1}};
        %% there are some tasks in the storage
        _Any ->
            {reply, {ok, Key}, State#state{next_task = NextTask}}
    end;

%% in case of incorrect Key
handle_call({ack_task, _Key}, _From, State = #state{}) ->
    {reply, {error, incorrect_key}, State};


handle_call(Request, _From, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
    {stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
