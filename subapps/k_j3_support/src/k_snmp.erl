-module(k_snmp).

-behaviour(gen_fsm).

-include("snmp_task.hrl").
-include_lib("k_storage/include/gateway.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_fsm_spec.hrl").

-define(USER, "user").
-define(TARGET, "just").

-define(destroy, 6).
-define(createAndWait, 5).
-define(createAndGo, 4).
-define(notReady, 3).
-define(notInService, 2).
-define(active, 1).

%% API
-export([
    start_link/0,
    get_column_val/2,
    set_row/3,
    set_row/1,
    del_row/2,
    del_row/1
]).

%% Callbacks
-export([
    init/1,

    ready/2,
    ready/3,
    sleep/2,
    sleep/3,

    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {
    time = 10000 :: integer()
}).

-record(varbind, {
    oid     :: list(),
    type    :: atom(),
    value   :: term(),
    int     :: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    process_queue(),
    {ok, Pid}.

-spec get_column_val(snmp_column_name(), snmp_index()) -> snmp_result().
get_column_val(ColumnName, Index) ->
    {ok, [OID]} = snmpm:name_to_oid(ColumnName),
    Result = snmpm:sync_get(?USER, ?TARGET, [OID ++ Index]),
    parse_snmp_result(Result).

-spec set_row(snmp_column_name(), snmp_index(), snmp_value_list()) -> ok.
set_row(TableName, Index, ValueList) ->
    Task = #task{function = fun ?MODULE:set_row/1, args = {TableName, Index, ValueList}},
    process_task(Task).

-spec del_row(snmp_column_name(), snmp_index()) -> ok.
del_row(TableName, Index)->
    Task = #task{function = fun ?MODULE:del_row/1, args = {TableName, Index}},
    process_task(Task).

%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

init(_Args) ->
    {ok, ready, #state{}}.

-spec sleep(process_queue, #state{}) -> {next_state, sleep, #state{}};
           (wake_up, #state{})       -> {next_state, ready, #state{}};
           (term(), #state{})        -> {stop, {bad_event, term()}, #state{}}.
sleep(process_queue, State = #state{}) ->
    {next_state, sleep, State};
sleep(wake_up, State = #state{}) ->
    process_queue(),
    {next_state, ready, State};
sleep(Event, State = #state{}) ->
    {stop, {bad_event, Event}, State}.

-spec ready(process_queue, #state{}) -> {next_state, ready, #state{}} | {next_state, sleep, #state{}};
           (term(), #state{})        -> {stop, {bad_event, term()}, #state{}}.
ready(process_queue, State = #state{time = Time}) ->
    case next() of
    {ok, #task{id = Id, function = Fun, args = Args}} ->
        case Fun(Args) of
            ok ->
                ack(Id),
                process_queue(),
                {next_state, ready, State};
            {error, _Reason} ->
                start_timer(Time),
                {next_state, sleep, State}
        end;
    {ok, []} ->
        {next_state, ready, State}
    end;
ready(Event, State = #state{}) ->
    {stop, {bad_event, Event}, State}.

-spec sleep(term(), term(), term()) ->
    {stop, {bad_sync_event, term()}, term()}.
sleep(Event, _From, State) ->
    {stop, {bad_sync_event, Event}, State}.

-spec ready(term(), term(), term()) ->
    {stop, {bad_sync_event, term()}, term()}.
ready(Event, _From, State) ->
    {stop, {bad_sync_event, Event}, State}.

handle_event(Event, _StateName, State = #state{}) ->
    {stop, {bad_event, Event}, State}.

handle_sync_event(Event, _From, _StateName, State = #state{}) ->
    {stop, {bad_sync_event, Event}, State}.

handle_info(_Info, StateName, State = #state{}) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State = #state{}) ->
    ok.

code_change(_OldVsn, StateName, State = #state{}, _Extra) ->
    {ok, StateName, State}.

%% ===================================================================
%% Internals
%% ===================================================================

%%% Queue functions

next() ->
    k_snmp_tasks_queue:next().

save_task(Task) ->
    k_snmp_tasks_queue:save(Task).

ack(Id) ->
    k_snmp_tasks_queue:ack(Id).
%%%

start_timer(Time) ->
    {ok, _TRef} = timer:apply_after(Time, gen_fsm, send_event, [?MODULE, wake_up]).

process_queue() ->
    gen_fsm:send_event(?MODULE, process_queue).

process_task(Task) ->
    save_task(Task),
    process_queue().

-spec set_row(tuple()) -> ok | {error, term()}.
set_row({TableName, Index, ValueList}) ->
    case is_exist(TableName, Index) of
        exist ->
            update(Index, ValueList);
        notExist ->
            create(TableName, Index, ValueList);
        incorrectState ->
            recreate(TableName, Index, ValueList);
        Unexpected ->
            ?log_warn("Not expected value: ~p", [Unexpected]),
            Unexpected
    end.

-spec del_row(tuple()) -> ok | {error, term()}.
del_row({TableName, Index}) ->
    case TableName of
        gtw ->
            set(Index, [{gtwStatus, ?destroy}]);
        sts ->
            set(Index, [{stsStatus, ?destroy}]);
        cst ->
            set(Index, [{cstStatus, ?destroy}]);
        cnn ->
            set(Index, [{cnnStatus, ?destroy}]);
        _Any ->
            {error, noSuchTable}
    end.

recreate(TableName, Index, ValueList) ->
    del_row({TableName, Index}),
    create(TableName, Index, ValueList).

create(TableName, Index, ValueList) ->
    case TableName of
        gtw ->
            update(Index, [{gtwStatus, ?createAndWait}] ++ ValueList ++ [{gtwStatus, ?active}]);
        sts ->
            update(Index, [{stsStatus, ?createAndWait}] ++ ValueList ++ [{stsStatus, ?active}]);
        cst ->
            update(Index, [{cstStatus, ?createAndWait}] ++ ValueList ++ [{cstStatus, ?active}]);
        cnn ->
            update(Index, [{cnnStatus, ?createAndWait}] ++ ValueList ++ [{cnnStatus, ?active}]);
        _Any ->
            {error, noSuchTable}
    end.

update(Index, ValueList) ->
    set(Index, ValueList).

set(_Index, []) ->
    ok;
set(Index, [{ColumnName, Value} | ValueList]) ->
    {ok, [OID]} = snmpm:name_to_oid(ColumnName),
    RowResult = snmpm:sync_set(?USER, ?TARGET, [{OID ++ Index, Value}]),
    case parse_snmp_result(RowResult) of
        {ok, _} ->
            set(Index, ValueList);
        {error, Reason} ->
            ?log_error("Set index: ~p column: ~p value: ~p failed with: ~p",
                [Index, ColumnName, Value, Reason]),
            {error, Reason}
    end.

is_exist(TableName, Index)->
    {ok, ColumnName} = status_column_name(TableName),
    case get_column_val(ColumnName, Index) of
        {ok, ?active} -> exist;
        {ok, Some} when is_integer(Some) -> incorrectState;
        {error, {timeout, T}} -> {error, {timeout, T}};
        {_Error, _More} -> notExist
    end.

status_column_name(TableName) ->
    case TableName of
        gtw -> {ok, gtwStatus};
        sts -> {ok, stsStatus};
        cst -> {ok, cstStatus};
        cnn -> {ok, cnnStatus};
        _Any -> {error, noSuchTable}
    end.

parse_snmp_result(Result) ->
    case Result of
        {ok, {noError, 0, [#varbind{value = Value}]}, _Remaining} ->
            case Value of
                noSuchObject ->
                    {error, noSuchObject};
                noSuchInstance ->
                    {error, noSuchInstance};
                AnyValue->
                    {ok, AnyValue}
            end;
        {ok, {ErState, ErInd, Varbind}, _Remaining} ->
            ?log_error("ErState: ~p, ErInd: ~p, Varbind: ~p",
                [ErState, ErInd, Varbind]),
            {error, ErState};
        {error, Reason} ->
            {error, Reason}
    end.
