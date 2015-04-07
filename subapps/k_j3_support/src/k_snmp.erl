-module(k_snmp).

-include_lib("alley_common/include/logging.hrl").

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
    get_column_val/2,
    set_row/3,
    del_row/2
]).

-record(varbind, {
    oid     :: list(),
    type    :: atom(),
    value   :: term(),
    int     :: integer()
}).

-type snmp_index() :: list().
-type snmp_column_name() :: atom().
-type snmp_value() :: term().
-type er_snmp_state() :: term().
-type snmp_error_reason() :: term().
-type snmp_value_list() :: [{snmp_column_name(), snmp_value()}].
-type snmp_result() :: {ok, snmp_value()} |
                        {error, noSuchObject} |
                        {error, noSuchInstance} |
                        {error, er_snmp_state()} |
                        {error, snmp_error_reason()}.

%% ===================================================================
%% API
%% ===================================================================

-spec get_column_val(snmp_column_name(), snmp_index()) -> snmp_result().
get_column_val(ColumnName, Index) ->
    {ok, [OID]} = snmpm:name_to_oid(ColumnName),
    Result = snmpm:sync_get(?USER, ?TARGET, [OID ++ Index]),
    parse_snmp_result(Result).

-spec set_row(snmp_column_name(), snmp_index(), snmp_value_list()) -> ok.
set_row(TableName, Index, ValueList) ->
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

-spec del_row(snmp_column_name(), snmp_index()) -> ok.
del_row(TableName, Index)->
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

%% ===================================================================
%% Internal
%% ===================================================================

recreate(TableName, Index, ValueList) ->
    del_row(TableName, Index),
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
