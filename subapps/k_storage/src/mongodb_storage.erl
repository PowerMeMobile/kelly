-module(mongodb_storage).

%% API
-export([
	start_link/2,
	find/3,
	find/4,
	find_one/3,
	find_one/4,
	upsert/4,
	delete/3,
	command/2,
	ensure_index/3
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

-include("application.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-record(state, {
	db_name :: binary(),
	db_pool :: resource_pool:pool()
}).

-type server_name() :: atom().
-type collection() :: atom() | binary().
-type plist() :: [{atom(), term()}].
-type selector() :: plist().
-type projector() :: plist().
-type modifier() :: plist().
-type key() :: term().
-type value() :: term().
-type command() :: tuple().
-type index_spec() :: tuple().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(server_name(), plist()) -> {ok, pid()}.
start_link(ServerName, Props) ->
	gen_server:start_link({local, ServerName}, ?MODULE, [Props], []).

-spec find(server_name(), collection(), selector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(ServerName, Coll, Selector) ->
	find(ServerName, Coll, Selector, []).

-spec find(server_name(), collection(), selector(), projector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(ServerName, Coll, Selector, Projector) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			Cursor = mongo:find(Coll, bson:document(Selector), bson:document(Projector)),
			Documents = mongo_cursor:rest(Cursor),
			Results = lists:map(
				fun(BsonDoc) ->
					BsonKey = bson:at('_id', BsonDoc),
					BsonValue = bson:exclude(['_id'], BsonDoc),
					{BsonKey, bson:fields(BsonValue)}
				end,
				Documents),
			mongo_cursor:close(Cursor),
			Results
		end
	).

-spec find_one(server_name(), collection(), selector()) ->
	{ok, Plist::[tuple()]} | {error, reason()}.
find_one(ServerName, Coll, Selector) ->
	find_one(ServerName, Coll, Selector, []).

-spec find_one(server_name(), collection(), selector(), projector()) ->
	{ok, Plist::[tuple()]} | {error, reason()}.
find_one(ServerName, Coll, Selector, Projector) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			case mongo:find_one(Coll, bson:document(Selector), bson:document(Projector)) of
				{} ->
					{error, no_entry};
				{BsonDoc} ->
					BsonValue = bson:exclude(['_id'],  BsonDoc),
					bson:fields(BsonValue)
			end
		end
	).

-spec upsert(server_name(), collection(), selector(), modifier()) ->
	ok | {error, reason()}.
upsert(ServerName, Coll, Selector, Modifier) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			mongo:repsert(Coll, bson:document(Selector), {'$set', bson:document(Modifier)})
		end
	).

-spec delete(server_name(), collection(), selector()) ->
	ok | {error, reason()}.
delete(ServerName, Coll, Selector) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			mongo:delete(Coll, bson:document(Selector))
		end
	).

-spec command(server_name(), command()) ->
	{ok, Result::term()} | {error, reason()}.
command(ServerName, Command) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			mongo:command(Command)
		end
	).

-spec ensure_index(server_name(), collection(), index_spec()) ->
	ok | {error, reason()}.
ensure_index(ServerName, Coll, IndexSpec) ->
	mongo_do(ServerName, safe, master,
		fun() ->
			mongo:create_index(Coll, IndexSpec)
		end
	).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Props]) ->
	?log_debug("init", []),
	{ok, DbName, DbPool} = connect(Props),
	{ok, #state{db_name = DbName, db_pool = DbPool}}.

handle_call(get_name_and_pool, _From, State = #state{
	db_name = DbName,
	db_pool = DbPool
}) ->
	{reply, {ok, DbName, DbPool}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{db_pool = DbPool}) ->
	resource_pool:close(DbPool),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

connect(Props) ->
	ConnProps = proplists:get_value(mongodb_conn_props, Props),
	DbName = proplists:get_value(mongodb_dbname, Props),
	PoolSize = proplists:get_value(mongodb_pool_size, Props),
	ConnFactory =
		case ConnProps of
			{single, HostPort} ->
				mongo:connect_factory(HostPort);
			{replica_set, RSProps} ->
				mongo:rs_connect_factory(RSProps)
		end,
	DbPool = resource_pool:new(ConnFactory, PoolSize),
	{ok, DbName, DbPool}.

mongo_do(ServerName, WriteMode, ReadMode, ActionFun) ->
	case gen_server:call(ServerName, get_name_and_pool) of
		{ok, DbName, DbPool} ->
			{ok, DbConn} = resource_pool:get(DbPool),
			case mongo:do(WriteMode, ReadMode, DbConn, DbName, ActionFun) of
				{ok, ok} ->
					ok;
				{ok, {error, Reason}} ->
					{error, Reason};
				{ok, Entries} ->
					{ok, Entries};
				{failure, {connection_failure, _} = Reason} ->
					?log_error("MongoDB connection failure: ~p", [Reason]),
					mongo_do(ServerName, WriteMode, ReadMode, ActionFun);
				{failure, {connection_failure, _,_} = Reason} ->
					?log_error("MongoDB connection failure: ~p", [Reason]),
					mongo_do(ServerName, WriteMode, ReadMode, ActionFun);
				{failure, Reason} ->
					?log_error("MongoDB failure: ~p", [Reason]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
