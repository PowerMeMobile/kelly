-module(k_dynamic_storage).

%% API
-export([
	start_link/0,
	find/2,
	find/3,
	find_one/2,
	find_one/3,
	upsert/3,
	update/3,
	delete/2,
	command/1,


	t/0
]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").

-type collection() :: atom() | binary().
-type plist() :: [{atom(), term()}].
-type selector() :: plist().
-type projector() :: plist().
-type modifier() :: plist().
-type key() :: term().
-type value() :: term().
-type command() :: tuple().
-type result() :: term().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	{ok, Props} = application:get_env(?APP, dynamic_storage),
	%% build db name in format YYYY-MM-00
	DbNameFmt = proplists:get_value(mongodb_dbname_fmt, Props),
	{{Year, Month, _}, _} = calendar:universal_time(),
	Date = lists:flatten(io_lib:format("~B-~2..0B", [Year, Month])),
	DbName = list_to_binary(io_lib:format(DbNameFmt, [Date])),
	%% set db name.
	NewProps = [{mongodb_dbname, DbName} | Props],
	{ok, Pid} = mongodb_storage:start_link(k_dynamic_storage, NewProps),
	{ok, Pid}.

t() ->
	mongodb_storage:ensure_index(k_dynamic_storage, mt_messages,
		{key, {ci, 1, ct, 1, imi, 1}, unique, true, dropDups, true}),
	mongodb_storage:ensure_index(k_dynamic_storage, mt_messages,
		{key, {qi, 1, omi, 1}}),
	mongodb_storage:ensure_index(k_dynamic_storage, mt_messages,
		{key, {rqt, 1}}),
	mongodb_storage:ensure_index(k_dynamic_storage, mo_messages,
		{key, {rqt, 1}}).

-spec find(collection(), selector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector) ->
	find(Coll, Selector, []).

-spec find(collection(), selector(), projector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector, Projector) ->
	mongodb_storage:find(k_dynamic_storage, Coll, Selector, Projector).

-spec find_one(collection(), selector()) ->
	{ok, plist()} | {error, reason()}.
find_one(Coll, Selector) ->
	find_one(Coll, Selector, []).

-spec find_one(collection(), selector(), projector()) ->
	{ok, plist()} | {error, reason()}.
find_one(Coll, Selector, Projector) ->
	mongodb_storage:find_one(k_dynamic_storage, Coll, Selector, Projector).

-spec upsert(collection(), selector(), modifier()) ->
	ok | {error, reason()}.
upsert(Coll, Selector, Modifier) ->
	mongodb_storage:upsert(k_dynamic_storage, Coll, Selector, Modifier).

-spec update(collection(), selector(), modifier()) ->
	ok | {error, reason()}.
update(Coll, Selector, Modifier) ->
	mongodb_storage:upsert(k_dynamic_storage, Coll, Selector, Modifier).

-spec delete(collection(), selector()) ->
	ok | {error, reason()}.
delete(Coll, Selector) ->
	mongodb_storage:delete(k_dynamic_storage, Coll, Selector).

-spec command(command()) ->
	{ok, result()} | {error, reason()}.
command(Command) ->
	mongodb_storage:command(k_dynamic_storage, Command).
