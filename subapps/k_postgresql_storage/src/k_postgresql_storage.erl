-module(k_postgresql_storage).

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

-define(PGSQL_POOL, pgsql_kelly_pool).
-define(KEY_LEN, 512).
-define(VAL_LEN, 32768).

-record(state, {}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(Table::string(), Opts::[tuple()]) -> {ok, string()} | {error, Reason::term()}.
open(Table, Opts) ->
	pgsql_do(
		fun(Conn) ->
			SelectSqlFmt =
				"SELECT tables.table_name FROM information_schema.tables WHERE table_name=$1",
			case pgsql:equery(Conn, SelectSqlFmt, [Table]) of
				{ok, _Cols, [{_Table}]} ->
					{ok, Table};
				{ok, _Columns, []} ->
					KeyLen = proplists:get_value(key_len, Opts, ?KEY_LEN),
					ValueLen = proplists:get_value(val_len, Opts, ?VAL_LEN),
					CreateTableSql = io_lib:format(
						"CREATE TABLE ~s (\
							id SERIAL PRIMARY KEY, \
							k VARCHAR(~p) UNIQUE, \
							v VARCHAR(~p)\
						)",
						[Table, KeyLen, ValueLen]),
					%?log_debug("~s", [CreateTableSql]),
					case pgsql:squery(Conn, CreateTableSql) of
						{ok, [], []} ->
							{ok, Table};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end
		end).

-spec close(Table::string()) -> ok | {error, Reason::term()}.
close(_Table) ->
	ok.

-spec read(Table::string()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read(Table) ->
	pgsql_do(
		fun(Conn) ->
			SelectSql = io_lib:format(
				"SELECT k, v FROM ~s", [Table]),
			%?log_debug("~s", [SelectSql]),
			case pgsql:equery(Conn, SelectSql) of
				{ok, _Cols, []} ->
					{ok, []};
				{ok, _Cols, Rows} ->
					KeyValuePairs = lists:map(
						fun({Key, Value}) ->
							{decode_key(Key), decode_value(Value)}
						end,
						Rows),
					{ok, KeyValuePairs};
				{error, Reason} ->
					{error, Reason}
			end
		end).

-spec read(Table::string(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Table, Key) ->
	pgsql_do(
		fun(Conn) ->
			SelectSqlFmt = io_lib:format(
				"SELECT v FROM ~s WHERE k=$1", [Table]),
			ParamKey = encode_key(Key),
			%?log_debug("~s (~p)", [SelectSqlFmt, ParamKey]),
			case pgsql:equery(Conn, SelectSqlFmt, [ParamKey]) of
				{ok, _Cols, []} ->
					{error, no_entry};
				{ok, _Cols, [{Value}|_]} ->
					{ok, decode_value(Value)};
				{error, Reason} ->
					{error, Reason}
			end
		end).

-spec write(Table::string(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Table, Key, Value) ->
	pgsql_do(
		fun(Conn) ->
			InsertSqlFmt = io_lib:format(
				"INSERT INTO ~s (k, v) VALUES ($1, $2)", [Table]),
			ParamKey = encode_key(Key),
			ParamValue = encode_value(Value),
			%?log_debug("~s (~p, ~p)", [InsertSqlFmt, ParamKey, ParamValue]),
			case pgsql:equery(Conn, InsertSqlFmt, [ParamKey, ParamValue]) of
				{ok, _Count} ->
					ok;
				{error, {error, error, <<"23505">>, _, _}} -> %% UNIQUE VIOLATION
					UpdateSqlFmt = io_lib:format(
						"UPDATE ~s SET v=$1 WHERE k=$2", [Table]),
					ParamValue = encode_value(Value),
					ParamKey = encode_key(Key),
					%?log_debug("~s (~p, ~p)", [UpdateSqlFmt, ParamValue, ParamKey]),
					case pgsql:equery(Conn, UpdateSqlFmt, [ParamValue, ParamKey]) of
						{ok, _Count} ->
							ok;
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end
		end).

-spec delete(Table::string(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Table, Key) ->
	pgsql_do(
		fun(Conn) ->
			DeleteSqlFmt = io_lib:format(
				"DELETE FROM ~s WHERE k=$1", [Table]),
			ParamKey = encode_key(Key),
			%?log_debug("~s (~p)", [DeleteSqlFmt, ParamKey]),
			case pgsql:equery(Conn, DeleteSqlFmt, [ParamKey]) of
				{ok, 0} ->
					{error, no_entry};
				{ok, _Count} ->
					ok;
				{error, Reason} ->
					{error, Reason}
   			end
		end).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	Args = application:get_all_env(?APP),
	?log_debug("starting pool...", []),
	ok = init_pooler(Args),
	?log_debug("pool started.", []),
	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok = application:stop(epgsql_pool).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec init_pooler(Args::[tuple()]) -> ok | {error, Reason::term()}.
init_pooler(Args) ->
	PoolSize = proplists:get_value(pool_size, Args),
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	Database = proplists:get_value(database, Args),
	Username = proplists:get_value(username, Args),
	Password = proplists:get_value(password, Args),
	Pool = {PoolSize, [
				{host, Host},
				{port, Port},
				{username, Username},
				{password, Password},
				{database, Database}
		   ]},
	application:set_env(epgsql_pool, pools, [?PGSQL_POOL]),
	application:set_env(epgsql_pool, ?PGSQL_POOL, Pool),
	ensure_app_started(epgsql_pool).

-spec ensure_app_started(atom()) -> ok | {error, term()}.
ensure_app_started(Application) ->
	case application:start(Application) of
		ok ->
			ok;
		{error, {not_started, Dependency}} ->
			case ensure_app_started(Dependency) of
				ok ->
					ensure_app_started(Application);
				Error ->
					Error
			end;
		{error, {already_started, Application}} ->
			ok;
		Error ->
			Error
	end.

pgsql_do(Fun) ->
	case pgsql_pool:get_connection(?PGSQL_POOL) of
		{error, Reason} ->
			{error, Reason};
		{ok, Conn} ->
				try Fun(Conn) of
					{error, disconnected} ->
						{error, no_storage};
					Result ->
						pgsql_pool:return_connection(?PGSQL_POOL, Conn),
						Result
				catch
					_Exception ->
						{error, no_storage}
				end
		end.

encode_key(Key) ->
	encode("~p.", Key).

encode_value(Value) ->
	encode("~p.", Value).

encode(Fmt, Value) ->
	lists:flatten(io_lib:format(Fmt, [Value])).

decode_key(Key) ->
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Key)),
    {ok, Term} = erl_parse:parse_term(Tokens),
	Term.

decode_value(Value) ->
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Value)),
    {ok, Term} = erl_parse:parse_term(Tokens),
	Term.
