-module(mongodb_storage).

%% API
-export([
	start_link/0,
	find/2,
	find/3,
	find_one/2,
	find_one/3,
	insert/2,
	upsert/3,
	delete/2,
	command/1
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

-type collection() :: atom() | binary().
-type plist() :: [{atom(), term()}].
-type selector() :: plist().
-type projector() :: plist().
-type key() :: term().
-type value() :: term().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec find(collection(), selector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector) ->
	find(Coll, Selector, []).

-spec find(collection(), selector(), projector()) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector, Projector) ->
	mongo_do(safe, master,
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

-spec find_one(collection(), selector()) ->
	{ok, Plist::[tuple()]} | {error, reason()}.
find_one(Coll, Selector) ->
	find_one(Coll, Selector, []).

-spec find_one(collection(), selector(), projector()) ->
	{ok, Plist::[tuple()]} | {error, reason()}.
find_one(Coll, Selector, Projector) ->
	mongo_do(safe, master,
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

-spec insert(collection(), Value::[tuple()]) ->
	ok | {error, reason()}.
insert(Coll, Plist) ->
	mongo_do(safe, master,
		fun() ->
			mongo:insert(Coll, bson:document(Plist))
		end
	).

-spec upsert(collection(), selector(), Value::[tuple()]) ->
	ok | {error, reason()}.
upsert(Coll, Selector, Plist) when is_list(Plist) ->
	mongo_do(safe, master,
		fun() ->
			mongo:repsert(Coll, bson:document(Selector), {'$set', bson:document(Plist)})
		end
	).

-spec delete(collection(), selector()) ->
	ok | {error, reason()}.
delete(Coll, Selector) ->
	mongo_do(safe, master,
		fun() ->
			mongo:delete(Coll, bson:document(Selector))
		end
	).

-spec command(Command::tuple()) ->
	{ok, Result::term()} | {error, reason()}.
command(Command) ->
	mongo_do(safe, master,
		fun() ->
			mongo:command(Command)
		end
	).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, DbName, DbPool} = connect(),
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

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

connect() ->
	{ok, ConnProps} = application:get_env(?APP, mongodb_conn_props),
	{ok, DbName} = application:get_env(?APP, mongodb_dbname),
	{ok, PoolSize} = application:get_env(?APP, mongodb_pool_size),
	ConnFactory =
		case ConnProps of
			{single, HostPort} ->
				mongo:connect_factory(HostPort);
			{replica_set, RSProps} ->
				mongo:rs_connect_factory(RSProps)
		end,
	DbPool = resource_pool:new(ConnFactory, PoolSize),
	{ok, DbName, DbPool}.

mongo_do(WriteMode, ReadMode, ActionFun) ->
	case gen_server:call(?MODULE, get_name_and_pool) of
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
					mongo_do(WriteMode, ReadMode, ActionFun);
				{failure, {connection_failure, _,_} = Reason} ->
					?log_error("MongoDB connection failure: ~p", [Reason]),
					mongo_do(WriteMode, ReadMode, ActionFun);
				{failure, Reason} ->
					?log_error("MongoDB failure: ~p", [Reason]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
