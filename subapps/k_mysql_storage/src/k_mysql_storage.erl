-module(k_mysql_storage).

-behaviour(k_gen_storage).

%% k_gen_storage callbacks
-export([
	start_link/0,
	open/2,
	close/1,
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
-include_lib("emysql/include/emysql.hrl").

-define(MYSQL_POOL, mysql_kelly_pool).
-define(KEY_LEN, 512).
-define(VAL_LEN, 32768).
-define(ENGINE, "MyISAM").

-record(state, {}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(Table::string(), Opts::[tuple()]) -> {ok, string()} | {error, Reason::term()}.
open(Table, Opts) ->
	SelectSqlFmt =
		"SELECT tables.table_name FROM information_schema.tables WHERE table_name=?",
	case emysql:execute(?MYSQL_POOL, SelectSqlFmt, [Table]) of
		#result_packet{rows = []} ->
			KeyLen = proplists:get_value(key_len, Opts, ?KEY_LEN),
			ValueLen = proplists:get_value(val_len, Opts, ?VAL_LEN),
			Engine = proplists:get_value(engine, Opts, ?ENGINE),
			CreateTableSql = io_lib:format(
				"CREATE TABLE ~s ( \
					k VARCHAR(~p), \
					v VARCHAR(~p), \
					PRIMARY KEY(k) \
				) ENGINE=~s",
				[Table, KeyLen, ValueLen, Engine]),
			%?log_debug("~s", [CreateTableSql]),
			case emysql:execute(?MYSQL_POOL, CreateTableSql) of
				#ok_packet{} ->
					{ok, Table};
				#error_packet{msg = Msg} ->
					{error, Msg}
			end;
		#result_packet{rows = [_Table|_]} ->
			{ok, Table};
		#error_packet{msg = Msg} ->
			{error, Msg}
	end.

-spec close(Table::string()) -> ok | {error, Reason::term()}.
close(_Table) ->
	ok.

-spec read(Table::string(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Table, Key) ->
	SelectSqlFmt = io_lib:format(
		"SELECT v FROM ~s WHERE k=?", [Table]),
	ParamKey = encode_key(Key),
	%?log_debug("~s (~p)", [SelectSqlFmt, ParamKey]),
	case emysql:execute(?MYSQL_POOL, SelectSqlFmt, [ParamKey]) of
		#result_packet{rows = []} ->
			{error, no_entry};
		#result_packet{rows = [[Value|_]]} ->
			{ok, decode_value(Value)};
		#error_packet{msg = Msg} ->
			{error, Msg}
	end.

-spec write(Table::string(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Table, Key, Value) ->
	InsertSqlFmt = io_lib:format(
		"INSERT INTO ~s (k, v) VALUES (?, ?)", [Table]),
	ParamKey = encode_key(Key),
	ParamValue = encode_value(Value),
	%?log_debug("~s (~p, ~p)", [InsertSqlFmt, ParamKey, ParamValue]),
	case emysql:execute(?MYSQL_POOL, InsertSqlFmt, [ParamKey, ParamValue]) of
		#ok_packet{} ->
			ok;
		#error_packet{code = 1062} -> %% UNIQUE VIOLATION
			UpdateSqlFmt = io_lib:format(
				"UPDATE ~s SET v=? WHERE k=?", [Table]),
			ParamValue = encode_value(Value),
			ParamKey = encode_key(Key),
			%?log_debug("~s (~p, ~p)", [UpdateSqlFmt, ParamValue, ParamKey]),
			case emysql:execute(?MYSQL_POOL, UpdateSqlFmt, [ParamValue, ParamKey]) of
				#ok_packet{} ->
					ok;
				#error_packet{msg = Msg} ->
					{error, Msg}
			end;
		#error_packet{msg = Msg} ->
			{error, Msg}
	end.

-spec delete(Table::string(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Table, Key) ->
	DeleteSqlFmt = io_lib:format(
		"DELETE FROM ~s WHERE k=?", [Table]),
	ParamKey = encode_key(Key),
	%?log_debug("~s (~p)", [DeleteSqlFmt, ParamKey]),
	case emysql:execute(?MYSQL_POOL, DeleteSqlFmt, [ParamKey]) of
		#ok_packet{affected_rows = 0} ->
			{error, no_entry};
		#ok_packet{} ->
			ok;
		#error_packet{msg = Msg} ->
			{error, Msg}
	end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	Args = application:get_all_env(?APP),
	?log_debug("starting pool...", []),
	init_pool(Args),
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

init_pool(Args) ->
	PoolSize = proplists:get_value(pool_size, Args),
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	Database = proplists:get_value(database, Args),
	Username = proplists:get_value(username, Args),
	Password = proplists:get_value(password, Args),
	ok = emysql:add_pool(?MYSQL_POOL, PoolSize, Username, Password, Host, Port, Database, utf8).

encode_key(Key) ->
	encode("~p.", Key).

encode_value(Value) ->
	encode("~p.", Value).

encode(Fmt, Value) ->
	lists:flatten(io_lib:format(Fmt, [Value])).

decode_value(Value) ->
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Value)),
    {ok, Term} = erl_parse:parse_term(Tokens),
	Term.
