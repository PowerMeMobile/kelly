-module(shifted_storage).

-export([
    find_one/2,
    find_one/3,

    find/2,
    find/3,
    find/5,

    command/1
]).

-include("application.hrl").

-type collection() :: atom() | binary().
-type key() :: term().
-type document() :: bson:document().
-type selector() :: bson:document().
-type projector() :: bson:document().
-type skip() :: non_neg_integer().
-type limit() :: undefined | non_neg_integer().
-type command() :: bson:document().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec find_one(collection(), selector()) ->
    {ok, document()} | {error, reason()}.
find_one(Coll, Selector) ->
    find_one(Coll, Selector, {}).

-spec find_one(collection(), selector(), projector()) ->
    {ok, document()} | {error, reason()}.
find_one(Coll, Selector, Projector) ->
    {ok, Shifts} = gen_storage_manager:get_shifts(),
    find_one(Shifts, Coll, Selector, Projector).

-spec find(collection(), selector()) ->
    {ok, [{key(), document()}]} | {error, reason()}.
find(Coll, Selector) ->
    find(Coll, Selector, {}).

-spec find(collection(), selector(), projector()) ->
    {ok, [{key(), document()}]} | {error, reason()}.
find(Coll, Selector, Projector) ->
    {ok, Shifts} = gen_storage_manager:get_shifts(),
    find_no_skip_limit(Shifts, Coll, Selector, Projector, []).

-spec find(collection(), selector(), projector(), skip(), limit()) ->
    {ok, [{key(), document()}]} | {error, reason()}.
find(Coll, Selector, Projector, Skip, Limit) ->
    {ok, Shifts} = gen_storage_manager:get_shifts(),
    find(lists:reverse(Shifts), Coll, Selector, Projector, Skip, Limit, []).

-spec command(command()) ->
    {ok, document()} | {error, reason()}.
command(Command) ->
    {ok, Shifts} = gen_storage_manager:get_shifts(),
    command(Shifts, Command, []).

%% ===================================================================
%% Internal
%% ===================================================================

find_one([], _Coll, _Selector, _Projector) ->
    {error, no_entry};
find_one([Shift | Shifts], Coll, Selector, Projector) ->
    {ok, Props} = application:get_env(?APP, shifted_storage),
    Props2 = [{mongodb_dbname, Shift} | Props],

    {ok, Pid} = mongodb_storage:start_link(Props2),
    Res = mongodb_storage:find_one(Pid, Coll, Selector, Projector),
    ok = mongodb_storage:stop(Pid),

    case Res of
        {ok, _} ->
            Res;
        {error, no_entry} ->
            find_one(Shifts, Coll, Selector, Projector)
    end.

find_no_skip_limit([], _Coll, _Selector, _Projector, Acc) ->
    {ok, Acc};
find_no_skip_limit([Shift | Shifts], Coll, Selector, Projector, Acc) ->
    {ok, Props} = application:get_env(?APP, shifted_storage),
    Props2 = [{mongodb_dbname, Shift} | Props],

    {ok, Pid} = mongodb_storage:start_link(Props2),
    Res = mongodb_storage:find(Pid, Coll, Selector, Projector),
    ok = mongodb_storage:stop(Pid),

    case Res of
        {ok, []} ->
            find_no_skip_limit(Shifts, Coll, Selector, Projector, Acc);
        {ok, Docs} ->
            Acc2 = Docs ++ Acc,
            find_no_skip_limit(Shifts, Coll, Selector, Projector, Acc2)
    end.

find([], _Coll, _Selector, _Projector, _Skip, _Limit, Acc) ->
    {ok, Acc};
find([Shift | Shifts], Coll, Selector, Projector, 0, Limit, Acc) ->
    {ok, Props} = application:get_env(?APP, shifted_storage),
    Props2 = [{mongodb_dbname, Shift} | Props],

    {ok, Pid} = mongodb_storage:start_link(Props2),
    Res = mongodb_storage:find(Pid, Coll, Selector, Projector, 0, Limit),
    ok = mongodb_storage:stop(Pid),

    case Res of
        {ok, []} ->
            find(Shifts, Coll, Selector, Projector, 0, Limit, Acc);
        {ok, Docs} ->
            Acc2 = Acc ++ Docs,
            case Limit - length(Docs) of
                0 ->
                    {ok, Acc2};
                Rest when Rest > 0 ->
                    find(Shifts, Coll, Selector, Projector, 0, Rest, Acc2)
            end
    end;
find([Shift | Shifts], Coll, Selector, Projector, Skip, Limit, Acc) ->
    {ok, Props} = application:get_env(?APP, shifted_storage),
    Props2 = [{mongodb_dbname, Shift} | Props],

    {ok, Pid} = mongodb_storage:start_link(Props2),
    Command = {
        'count', atom_to_binary(Coll, latin1),
        Selector %% should be in {'$query, Query, ...} form
    },
    Res =
        case mongodb_storage:command(Pid, Command) of
            %% collection is missing
            {ok, {missing, true, n, 0.0, ok, 1.0}} ->
                {ok, [], Skip};
            {ok, {n, 0.0, ok, 1.0}} ->
                {ok, [], Skip};
            {ok, {n, Count, ok, 1.0}} ->
                Count2 = round(Count),
                case Count2 =< Skip of
                    true ->
                        {ok, [], Skip - Count2};
                    false ->
                        case mongodb_storage:find(Pid, Coll, Selector, Projector, Skip, Limit) of
                            {ok, Docs} ->
                                {ok, Docs, 0};
                            Error ->
                                Error
                        end
                end
        end,
    ok = mongodb_storage:stop(Pid),

    case Res of
        {ok, Docs2, Skip2} ->
            Acc2 = Acc ++ Docs2,
            case Limit - length(Docs2) of
                0 ->
                    {ok, Acc2};
                Rest when Rest > 0 ->
                    find(Shifts, Coll, Selector, Projector, Skip2, Rest, Acc2)
            end;
        Error2 ->
            Error2
    end.

command([], _Command, Acc) ->
    {ok, Acc};
command([Shift | Shifts], Command, Acc) ->
    {ok, Props} = application:get_env(?APP, shifted_storage),
    Props2 = [{mongodb_dbname, Shift} | Props],

    {ok, Pid} = mongodb_storage:start_link(Props2),
    Res = mongodb_storage:command(Pid, Command),
    ok = mongodb_storage:stop(Pid),

    case Res of
        {ok, {results, Docs, _, _, _, _, ok, 1.0}} ->
            NewAcc = Docs ++ Acc,
            command(Shifts, Command, NewAcc);
        {ok, {result, Docs, ok, 1.0}} ->
            NewAcc = Docs ++ Acc,
            command(Shifts, Command, NewAcc);
        {error, {ok, 0.0, errmsg, <<"ns doesn't exist">>}} ->
            %% shift db doesn't exist.
            command(Shifts, Command, Acc);
        Error ->
            Error
    end.
