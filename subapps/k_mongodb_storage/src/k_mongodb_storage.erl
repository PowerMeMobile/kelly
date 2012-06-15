-module(k_mongodb_storage).

-behaviour(k_gen_storage).

%% k_gen_storage callbacks
-export([
	start_link/0,
	open/2,
	close/1,
	read/1,
	read/2,
	write/3,
	delete/2
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
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-record(state, {
	conn_props,
	pool_size :: integer(),
	conn_pool :: pid(),
	db_name :: binary()
}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(CollectionName::string(), Opts::[tuple()]) -> {ok, Coll::binary()} | {error, Reason::term()}.
open(CollectionName, _Opts) ->
	{ok, list_to_binary(CollectionName)}.

-spec close(Coll::binary()) -> ok | {error, Reason::term()}.
close(_Coll) ->
	ok.

-spec read(Coll::binary()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read(Coll) ->
	case gen_server:call(?MODULE, get_conn_and_db, infinity) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					Cursor = mongo:find(Coll, {}),
					Documents = mongo_cursor:rest(Cursor),
					lists:map(
						fun({'_id', BsonBinKey, 'value', BsonBinValue}) ->
							{bson_bin_to_term(BsonBinKey), bson_bin_to_term(BsonBinValue)}
						end,
						Documents)
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

-spec read(Coll::binary(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Coll, Key) ->
	case gen_server:call(?MODULE, get_conn_and_db, infinity) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					case mongo:find_one(Coll, selector(Key)) of
						{} ->
							{error, no_entry};
						{{'_id', _, 'value', BsonBinValue}} ->
							{ok, bson_bin_to_term(BsonBinValue)}
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

-spec write(Coll::binary(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Coll, Key, Value) ->
	case gen_server:call(?MODULE, get_conn_and_db, infinity) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					mongo:repsert(Coll, selector(Key), {'_id', term_to_bson_bin(Key), 'value', term_to_bson_bin(Value)})
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

-spec delete(Coll::binary(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Coll, Key) ->
	case gen_server:call(?MODULE, get_conn_and_db, infinity) of
		{ok, Conn, DBName} ->
			Res = mongo:do(safe, master, Conn, DBName,
				fun() ->
					mongo:delete(Coll, selector(Key))
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

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, ConnProps} = application:get_env(?APP, conn_props),
	{ok, PoolSize} = application:get_env(?APP, pool_size),
	{ok, DBName} = application:get_env(?APP, dbname),
	ConnFactory = case ConnProps of
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
	Reply = case resource_pool:get(Pool) of
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

selector(Key) -> {'_id', term_to_bson_bin(Key)}.

-spec term_to_bson_bin(Term::term()) -> bson:bin().
term_to_bson_bin(Term) ->
	{bin, bin, term_to_binary(Term)}.

-spec bson_bin_to_term(BsonBin::bson:bin()) -> term().
bson_bin_to_term(BsonBin) ->
	{bin, bin, Bin} = BsonBin,
	binary_to_term(Bin).
