-module(k_static_storage).

%% API
-export([
	start_link/0,
	find/2,
	find/3,
	find_one/2,
	find_one/3,
	upsert/3,
	delete/2,
	command/1
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
	{ok, Props} = application:get_env(?APP, static_storage),
	mongodb_storage:start_link(k_static_storage, Props).

-spec find(collection(), selector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector) ->
	find(Coll, Selector, []).

-spec find(collection(), selector(), projector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector, Projector) ->
	mongodb_storage:find(k_static_storage, Coll, Selector, Projector).

-spec find_one(collection(), selector()) ->
	{ok, plist()} | {error, reason()}.
find_one(Coll, Selector) ->
	find_one(Coll, Selector, []).

-spec find_one(collection(), selector(), projector()) ->
	{ok, plist()} | {error, reason()}.
find_one(Coll, Selector, Projector) ->
	mongodb_storage:find_one(k_static_storage, Coll, Selector, Projector).

-spec upsert(collection(), selector(), modifier()) ->
	ok | {error, reason()}.
upsert(Coll, Selector, Modifier) ->
	mongodb_storage:upsert(k_static_storage, Coll, Selector, Modifier).

-spec delete(collection(), selector()) ->
	ok | {error, reason()}.
delete(Coll, Selector) ->
	mongodb_storage:delete(k_static_storage, Coll, Selector).

-spec command(command()) ->
	{ok, result()} | {error, reason()}.
command(Command) ->
	mongodb_storage:command(k_static_storage, Command).
