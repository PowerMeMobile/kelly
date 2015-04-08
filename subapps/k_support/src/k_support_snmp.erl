-module(k_support_snmp).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    sync_get/2,
    sync_set/2
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

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").

-record(varbind, {
    oid     :: list(),
    type    :: atom(),
    value   :: term(),
    int     :: integer()
}).

-type snmp_user() :: list().
-type snmp_target() :: list().
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

-define(PMM_OID, [1,3,6,1,4,1,20789]).
%% > snmpm:name_to_oid(powerMeMobile).
%% {ok,[[1,3,6,1,4,1,20789]]}
%% https://github.com/PowerMeMobile/funnel_mini/blob/master/mibs/FUNNEL-MIB.mib#L10
%% https://github.com/PowerMeMobile/just_mini/blob/master/mibs/JUST-MIB.mib#L10

-record(state, {
    targets = [] :: [{snmp_user(), snmp_target()}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec sync_get(snmp_user(), [snmp_oid()]) -> snmp_result().
sync_get(User, Oids) ->
    case gen_server:call(?MODULE, {get_target, User}, 30000) of
        {ok, Target} ->
            Result = snmpm:sync_get(User, Target, Oids),
            parse_snmp_get_result(Result);
        {error, Error} ->
            {error, Error}
    end.

-spec sync_set(snmp_user(), [{snmp_oid(), snmp_value()}]) -> snmp_result().
sync_set(User, Values) ->
    case gen_server:call(?MODULE, {get_target, User}, 30000) of
        {ok, Target} ->
            Result = snmpm:sync_set(User, Target, Values),
            parse_snmp_set_result(Result);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({get_target, User}, _From, State = #state{targets = Targets}) ->
    UserTargets =
        case proplists:get_value(User, Targets) of
            undefined ->
                snmpm:which_agents(User);
            Value ->
                Value
        end,
    case choose_target(User, UserTargets) of
        {ok, Target} ->
            UserTargets2 = [Target | lists:delete(Target, UserTargets)],
            Targets2 = [{User, UserTargets2} | proplists:delete(User, Targets)],
            {reply, {ok, Target}, State#state{targets = Targets2}};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

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

%% ===================================================================
%% Internal
%% ===================================================================

choose_target(_User, []) ->
    {error, noTarget};
choose_target(User, [Target | Targets]) ->
    case snmpm:sync_get(User, Target, [?PMM_OID]) of
        {error, {timeout, _}} ->
            choose_target(User, Targets);
        {ok, {noError, _, _}, _} ->
            {ok, Target}
    end.

parse_snmp_get_result(Result) ->
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

parse_snmp_set_result(Result) ->
    case Result of
        {ok, {noError, 0, _PrevValues}, _Remaining} ->
            ok;
        {ok, {ErState, ErInd, Varbind}, _Remaining} ->
            ?log_error("ErState: ~p, ErInd: ~p, Varbind: ~p",
                [ErState, ErInd, Varbind]),
            {error, ErState};
        {error, Reason} ->
            {error, Reason}
    end.
