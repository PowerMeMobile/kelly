-module(k_shifted_storage).

-export([
	find_one/2,
	find/2,
	command/1
]).

-include("application.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/customer.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

find_one(Coll, Selector) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	find_one(Shifts, Coll, Selector).

find(Coll, Selector) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	find(Shifts, Coll, Selector, []).

command(Command) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	command(Shifts, Command, []).

%% ===================================================================
%% Internal
%% ===================================================================

find_one([], _Coll, _Selector) ->
	{error, no_entry};
find_one([ShiftDbName|Shifts], Coll, Selector) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:find_one(Pid, Coll, Selector),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, _} ->
			Res;
		{error, no_entry} ->
			find_one(Shifts, Coll, Selector)
	end.

find([], _Coll, _Selector, []) ->
	{error, no_entry};
find([], _Coll, _Selector, Acc) ->
	{ok, Acc};
find([ShiftDbName|Shifts], Coll, Selector, Acc) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:find(Pid, Coll, Selector),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, Docs} ->
			NewAcc = Docs ++ Acc,
			find(Shifts, Coll, Selector, NewAcc);
		{error, no_entry} ->
			find(Shifts, Coll, Selector, Acc)
	end.

command([], _Command, Acc) ->
	{ok, Acc};
command([ShiftDbName|Shifts], Command, Acc) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:command(Pid, Command),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, {results, Docs, _, _, _, _, ok, 1.0}} ->
			NewAcc = Docs ++ Acc,
			command(Shifts, Command, NewAcc);
		Error ->
			Error
	end.
