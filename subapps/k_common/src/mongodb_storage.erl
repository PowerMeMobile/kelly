-module(mongodb_storage).

%% API
-export([
	start_link/1,
	find/2,
	find/3,
	find_one/2,
	find_one/3,
	upsert/3,
	delete/2,
	command/2
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
-include("gen_server_spec.hrl").
-include("logging.hrl").

-record(state, {
	conn_props,
	pool_size :: integer(),
	conn_pool :: pid(),
	db_name :: binary()
}).

-type key() :: term().
-type value() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(Coll::atom()) -> {ok, pid()}.
start_link(Coll) ->
	gen_server:start_link({local, Coll}, ?MODULE, [], []).

-spec find(Coll::binary(), Selector::[{atom(), term()}]) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector) ->
	find(Coll, Selector, []).

-spec find(Coll::binary(), Selector::[{atom(), term()}], Projector::[{atom(), term()}]) ->
	{ok, [{key(), value()}]} | {error, reason()}.
find(Coll, Selector, Projector) ->
	case gen_server:call(Coll, get_conn_and_db) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
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
				end),
			case Res of
				{ok, Entries} ->
					{ok, Entries};
				{failure, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec find_one(Coll::binary(), Selector::[{atom(), term()}]) ->
	{ok, Plist::[tuple()]} | {error, no_entry} | {error, reason()}.
find_one(Coll, Selector) ->
	find_one(Coll, Selector, []).

-spec find_one(Coll::binary(), Selector::[{atom(), term()}], Projector::[{atom(), term()}]) ->
	{ok, Plist::[tuple()]} | {error, no_entry} | {error, reason()}.
find_one(Coll, Selector, Projector) ->
	case gen_server:call(Coll, get_conn_and_db) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					case mongo:find_one(Coll, bson:document(Selector), bson:document(Projector)) of
						{} ->
							{error, no_entry};
						{BsonDoc} ->
							BsonValue = bson:exclude(['_id'],  BsonDoc),
							{ok, bson:fields(BsonValue)}
					end
				end),
			case Res of
				{ok, RetValue} ->
					RetValue;
				{failure, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec upsert(Coll::binary(), Selector::[{atom(), term()}], Value::[tuple()]) ->
	ok | {error, reason()}.
upsert(Coll, Selector, Plist) when is_list(Plist) ->
	case gen_server:call(Coll, get_conn_and_db) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					mongo:repsert(Coll, bson:document(Selector), {'$set', bson:document(Plist)})
				end),
			case Res of
				{ok, _} ->
					ok;
				{failure, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec delete(Coll::binary(), Selector::[{atom(), term()}]) ->
	ok | {error, no_entry} | {error, reason()}.
delete(Coll, Selector) ->
	case gen_server:call(Coll, get_conn_and_db) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					mongo:delete(Coll, bson:document(Selector))
				end),
			case Res of
				{ok, _} ->
					ok;
				{failure, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec command(Coll::binary(), Command::tuple()) ->
	{ok, Result::term()} | {error, reason()}.
command(Coll, Command) ->
	case gen_server:call(Coll, get_conn_and_db) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					mongo:command(Command)
				end),
			case Res of
				{ok, Result} ->
					{ok, Result};
				{failure, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, ConnProps} = application:get_env(?APP, mongodb_conn_props),
	{ok, PoolSize} = application:get_env(?APP, mongodb_pool_size),
	{ok, DBName} = application:get_env(?APP, mongodb_dbname),
	ConnFactory =
		case ConnProps of
			{single, HostPort} ->
				mongo:connect_factory(HostPort);
			{replica_set, RSProps} ->
				mongo:rs_connect_factory(RSProps)
		 end,
	Pool = resource_pool:new(ConnFactory, PoolSize),
	{ok, #state{
		conn_props = ConnProps,
		pool_size = PoolSize,
		conn_pool = Pool,
		db_name = DBName
	}}.

handle_call(get_conn_and_db, _From, State = #state{
	conn_pool = Pool,
	db_name = DBName
}) ->
	Reply =
		case resource_pool:get(Pool) of
			{ok, Conn} ->
				{ok, Conn, DBName};
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Reply, State};

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
