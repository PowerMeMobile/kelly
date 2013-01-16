-module(mongodb_storage).

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
	db_conn :: pid(),
	is_rs :: boolean()
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
	{ok, DBConn, DBName, IsRs} = connect(),
	{ok, #state{db_conn = DBConn, db_name = DBName, is_rs = IsRs}}.

handle_call(get_conn_and_name, _From, State = #state{
	db_conn = DBConn,
	db_name = DBName
}) ->
	{reply, {ok, DBConn, DBName}, State};

handle_call({try_reconnect, CurDBConn}, _From, #state{db_conn = CurDBConn, is_rs = IsRs}) ->
	%% ?log_info("Trying to reconnect to MongoDB...", []),
	%% close connection(s).
	case IsRs of
		true -> mongo:rs_disconnect(CurDBConn);
		false -> mongo:disconnect(CurDBConn)
	end,
	%% connect again.
	{ok, NewDBConn, NewDBName, NewIsRs} = connect(),
	%% ?log_info("Reconnected to MongoDB", []),
	{reply, ok, #state{db_conn = NewDBConn, db_name = NewDBName, is_rs = NewIsRs}};

handle_call({try_reconnect, PrevDBConn}, _From, State = #state{db_conn = CurDBConn}) when PrevDBConn =/= CurDBConn ->
	%% ?log_info("Already reconnecting(ed) to MongoDB.", []),
	{reply, ok, State};

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
	{ok, DBName} = application:get_env(?APP, mongodb_dbname),
	{DBConn, IsRs} =
		case ConnProps of
			{single, HostPort} ->
				{mongo:connect(HostPort), false};
			{replica_set, RSProps} ->
				{mongo:rs_connect(RSProps), true}
		 end,
	{ok, DBConn, DBName, IsRs}.

mongo_do(WriteMode, ReadMode, ActionFun) ->
	case gen_server:call(?MODULE, get_conn_and_name) of
		{ok, DBConn, DBName} ->
			case mongo:do(WriteMode, ReadMode, DBConn, DBName, ActionFun) of
				{ok, ok} ->
					ok;
				{ok, {error, Reason}} ->
					{error, Reason};
				{ok, Entries} ->
					{ok, Entries};
				{failure, {connection_failure, _} = Reason} ->
					?log_error("MongoDB connection failure: ~p", [Reason]),
					ok = gen_server:call(?MODULE, {try_reconnect, DBConn}),
					mongo_do(WriteMode, ReadMode, ActionFun);
				{failure, Reason} ->
					?log_error("MongoDB failure: ~p", [Reason]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
