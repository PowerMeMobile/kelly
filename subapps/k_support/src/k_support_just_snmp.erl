-module(k_support_just_snmp).

-include_lib("alley_common/include/logging.hrl").

-define(JUST_USER, "just_user").

-define(destroy, 6).
-define(createAndWait, 5).
-define(createAndGo, 4).
-define(notReady, 3).
-define(notInService, 2).
-define(active, 1).

%% API
-export([
    get_row_val/2,
    set_row/3,
    del_row/2
]).

-type snmp_index() :: list().
-type snmp_column_name() :: atom().
-type snmp_value() :: term().
-type er_snmp_state() :: term().
-type snmp_error_reason() :: term().
-type snmp_value_list() :: [{snmp_column_name(), snmp_value()}].
-type snmp_result() :: {ok, snmp_value()} |
                        {error, noTarget} |
                        {error, noSuchObject} |
                        {error, noSuchInstance} |
                        {error, er_snmp_state()} |
                        {error, snmp_error_reason()}.

%% ===================================================================
%% API
%% ===================================================================

-spec get_row_val(snmp_column_name(), snmp_index()) -> snmp_result().
get_row_val(ColumnName, Index) ->
    {ok, [OID]} = snmpm:name_to_oid(ColumnName),
    k_support_snmp:sync_get(?JUST_USER, [OID ++ Index]).

-spec set_row(snmp_column_name(), snmp_index(), snmp_value_list()) -> ok.
set_row(TableName, Index, ValueList) ->
    case is_exist(TableName, Index) of
        {ok, exist} ->
            update(Index, ValueList);
        {ok, notExist} ->
            create(TableName, Index, ValueList);
        {ok, incorrectState} ->
            recreate(TableName, Index, ValueList);
        {error, noTarget} ->
            {error, noTarget}
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

set(Index, Values) ->
    Fun = fun({ColName, Value}) ->
        {ok, [OID]} = snmpm:name_to_oid(ColName),
        {OID ++ Index, Value}
    end,
    Values2 = [Fun(V) || V <- Values],
    k_support_snmp:sync_set(?JUST_USER, Values2).

is_exist(TableName, Index)->
    {ok, ColumnName} = status_column_name(TableName),
    case get_row_val(ColumnName, Index) of
        {ok, ?active} -> {ok, exist};
        {ok, Some} when is_integer(Some) -> {ok, incorrectState};
        {error, noTarget} -> {error, noTarget};
        {_Error, _More} -> {ok, notExist}
    end.

status_column_name(TableName) ->
    case TableName of
        gtw -> {ok, gtwStatus};
        sts -> {ok, stsStatus};
        cst -> {ok, cstStatus};
        cnn -> {ok, cnnStatus};
        _Any -> {error, noSuchTable}
    end.
