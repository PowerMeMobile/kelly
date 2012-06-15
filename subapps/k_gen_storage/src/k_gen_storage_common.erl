-module(k_gen_storage_common).

-behaviour(gen_server).

%% API
-export([
	start_link/1,
	%% non-versioned
	read/1,
	read/2,
	write/3,
	delete/2,
	%% versioned
	read_version/2,
	read_version/3,
	write_version/4,
	delete_version/3
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

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-record(state, {
	db :: k_gen_server:handle()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(CollectionName::atom()) -> {ok, pid()}.
start_link(CollectionName) ->
	gen_server:start_link({local, CollectionName}, ?MODULE, [CollectionName], []).

%% Non-versioned

-spec read(CollectionName::atom()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read(CollectionName) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	k_gen_storage:read(Db).

-spec read(CollectionName::atom(), Key::term()) -> {ok, Value::term()} | {error, Reason::term()}.
read(CollectionName, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	k_gen_storage:read(Db, Key).

-spec write(CollectionName::atom(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(CollectionName, Key, Value) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	k_gen_storage:write(Db, Key, Value).

-spec delete(CollectionName::atom(), Key::term()) -> ok | {error, Reason::term()}.
delete(CollectionName, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	k_gen_storage:delete(Db, Key).

%% Versioned

-spec read_version(CollectionName::atom(), Version::integer()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read_version(CollectionName, Version) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	case k_gen_storage:read(Db) of
		{ok, Entries} ->
			{ok, lists:map(
				fun({Key, VersionedValue}) ->
					case VersionedValue of
						{Version, Value} ->
							{Key, Value};
						{OldVersion, OldValue} ->
							{Key, update(OldVersion, OldValue, Version)}
					end
				end,
				Entries)};
		Error ->
			Error
	 end.

-spec read_version(CollectionName::atom(), Version::integer(), Key::term()) -> {ok, Value::term()} | {error, Reason::term()}.
read_version(CollectionName, Version, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	case k_gen_storage:read(Db, Key) of
		{ok, {Version, Value}} ->
			{ok, Value};
		{ok, {OldVersion, OldValue}} when is_integer(OldVersion) ->
			update(OldVersion, OldValue, Version);
		Error ->
			Error
	 end.

-spec write_version(CollectionName::atom(), Version::integer(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write_version(CollectionName, Version, Key, Value) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	VValue = {Version, Value},
	k_gen_storage:write(Db, Key, VValue).

-spec delete_version(CollectionName::atom(), Version::integer(), Key::term()) -> ok | {error, Reason::term()}.
delete_version(CollectionName, _Version, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	k_gen_storage:delete(Db, Key).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([CollectionName]) ->
	{ok, Application} = application:get_application(),
	{ok, StorageName} = application:get_env(Application, CollectionName),
	{ok, Db} = k_gen_storage:open(StorageName),

	{ok, #state{
		db = Db
	}}.

handle_call(get_db, _From, State = #state{db = Db}) ->
	{reply, {ok, Db}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{
	db = Db
}) ->
	k_gen_storage:close(Db).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec update(OldVersion::integer(), OldValue::term(), NewVersion::integer()) -> {ok, NewValue::term()} | {error, Reason::term()}.
update(_OldVersion, OldValue, _NewVersion) ->
	{ok, OldValue}.
