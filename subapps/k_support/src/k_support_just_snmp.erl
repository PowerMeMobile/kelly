-module(k_support_just_snmp).

-include_lib("alley_common/include/logging.hrl").

-define(USER, "just").

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
-type snmp_value_list() :: [{snmp_column_name(), snmp_value()}].
-type snmp_result() :: {ok, snmp_value()} |
                        {error, no_target} |
                        {error, no_object} |
                        {error, no_instance} |
                        {error, not_found}.

%% ===================================================================
%% API
%% ===================================================================

-spec get_row_val(snmp_column_name(), snmp_index()) -> snmp_result().
get_row_val(ColName, Index) ->
    {ok, OID} = k_support_snmp:name_to_oid(ColName),
    case k_support_snmp:sync_get(?USER, [OID ++ Index]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            ?log_error("Get index: ~p column: ~p failed with: ~p",
                [Index, ColName, Reason]),
            {error, Reason}
    end.

-spec set_row(snmp_column_name(), snmp_index(), snmp_value_list()) -> ok.
set_row(TabName, Index, Values) ->
    case is_exist(TabName, Index) of
        {ok, exist} ->
            update(Index, Values);
        {ok, not_found} ->
            create(TabName, Index, Values);
        {ok, bad_state} ->
            recreate(TabName, Index, Values);
        {error, no_target} ->
            {error, no_target}
    end.

-spec del_row(snmp_column_name(), snmp_index()) -> ok.
del_row(TabName, Index)->
    case TabName of
        gtw ->
            set(Index, [{gtwStatus, ?destroy}]);
        sts ->
            set(Index, [{stsStatus, ?destroy}]);
        cst ->
            set(Index, [{cstStatus, ?destroy}]);
        cnn ->
            set(Index, [{cnnStatus, ?destroy}])
    end.

%% ===================================================================
%% Internal
%% ===================================================================

recreate(TabName, Index, Values) ->
    del_row(TabName, Index),
    create(TabName, Index, Values).

create(TabName, Index, Values) ->
    case TabName of
        gtw ->
            update(Index, [{gtwStatus, ?createAndWait}] ++ Values ++ [{gtwStatus, ?active}]);
        sts ->
            update(Index, [{stsStatus, ?createAndWait}] ++ Values ++ [{stsStatus, ?active}]);
        cst ->
            update(Index, [{cstStatus, ?createAndWait}] ++ Values ++ [{cstStatus, ?active}]);
        cnn ->
            update(Index, [{cnnStatus, ?createAndWait}] ++ Values ++ [{cnnStatus, ?active}])
    end.

update(Index, Values) ->
    set(Index, Values).

set(_Index, []) ->
    ok;
set(Index, [{ColName, Value} | Values]) ->
    {ok, OID} = k_support_snmp:name_to_oid(ColName),
    case k_support_snmp:sync_set(?USER, [{OID ++ Index, Value}]) of
        ok ->
            set(Index, Values);
        {error, Reason} ->
            ?log_error("Set index: ~p column: ~p value: ~p failed with: ~p",
                [Index, ColName, Value, Reason]),
            {error, Reason}
    end.

is_exist(TabName, Index)->
    {ok, ColName} = status_column_name(TabName),
    case get_row_val(ColName, Index) of
        {ok, ?active} -> {ok, exist};
        {ok, Some} when is_integer(Some) -> {ok, bad_state};
        {error, no_target} -> {error, no_target};
        {_Error, _More} -> {ok, not_found}
    end.

status_column_name(TabName) ->
    case TabName of
        gtw -> {ok, gtwStatus};
        sts -> {ok, stsStatus};
        cst -> {ok, cstStatus};
        cnn -> {ok, cnnStatus}
    end.
