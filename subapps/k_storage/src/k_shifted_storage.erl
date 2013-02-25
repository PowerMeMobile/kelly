-module(k_shifted_storage).

-export([
	find_one/2,
	find_one/3,

	find/2,
	find/3,

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
	find_one(Coll, Selector, {}).

find_one(Coll, Selector, Projector) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	find_one(Shifts, Coll, Selector, Projector).

find(Coll, Selector) ->
	find(Coll, Selector, {}).

find(Coll, Selector, Projector) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	find(Shifts, Coll, Selector, Projector, []).

command(Command) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	command(Shifts, Command, []).

%% ===================================================================
%% Internal
%% ===================================================================

find_one([], _Coll, _Selector, _Projector) ->
	{error, no_entry};
find_one([ShiftDbName|Shifts], Coll, Selector, Projector) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:find_one(Pid, Coll, Selector, Projector),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, _} ->
			Res;
		{error, no_entry} ->
			find_one(Shifts, Coll, Selector, Projector)
	end.

find([], _Coll, _Selector, _Projector, []) ->
	{ok, []};
find([], _Coll, _Selector, _Projector, Acc) ->
	{ok, Acc};
find([ShiftDbName|Shifts], Coll, Selector, Projector, Acc) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:find(Pid, Coll, Selector, Projector),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, Docs} ->
			NewAcc = Docs ++ Acc,
			find(Shifts, Coll, Selector, Projector, NewAcc);
		{error, no_entry} ->
			find(Shifts, Coll, Selector, Projector, Acc)
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
