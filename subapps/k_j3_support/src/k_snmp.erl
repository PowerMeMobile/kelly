-module(k_snmp).

-include_lib("alley_common/include/logging.hrl").

%% API
-export([
    sync_get/2,
    sync_set/2
]).

-record(varbind, {
    oid     :: list(),
    type    :: atom(),
    value   :: term(),
    int     :: integer()
}).

-type snmp_user() :: list().
-type snmp_oid() :: [byte()].

-type snmp_value() :: term().
-type er_snmp_state() :: term().
-type snmp_error_reason() :: term().
-type snmp_result() :: {ok, snmp_value()} |
                        {error, noAgent} |
                        {error, noSuchObject} |
                        {error, noSuchInstance} |
                        {error, er_snmp_state()} |
                        {error, snmp_error_reason()}.

%% ===================================================================
%% API
%% ===================================================================

-spec sync_get(snmp_user(), [snmp_oid()]) -> snmp_result().
sync_get(User, Oids) ->
    sync_get(User, snmpm:which_agents(User), Oids).

-spec sync_set(snmp_user(), [{snmp_oid(), snmp_value()}]) -> snmp_result().
sync_set(User, Values) ->
    sync_set(User, snmpm:which_agents(User), Values).

%% ===================================================================
%% Internal
%% ===================================================================

sync_get(_User, [], _Oids) ->
    {error, noAgent};
sync_get(User, [Target|Targets], Oids) ->
    case snmpm:sync_get(User, Target, Oids) of
        {error, {timeout, _}} ->
            sync_get(User, Targets, Oids);
        Result ->
            parse_snmp_result(Result)
    end.

sync_set(_User, [], _Values) ->
    {error, noAgent};
sync_set(User, [Target|Targets], Values) ->
    case snmpm:sync_set(User, Target, Values) of
        {error, {timeout, _}} ->
            sync_set(User, Targets, Values);
        Result ->
            parse_snmp_result(Result)
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
