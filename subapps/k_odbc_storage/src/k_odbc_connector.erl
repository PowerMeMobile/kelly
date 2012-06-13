-module(k_odbc_connector).

-export([
	start_link/2
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

-define(KLEN, 512).
-define(VLEN, 32768).

-record(state, {
	conn_ref :: odbc:connection_reference()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(ConnStr::string(), Opts::[tuple()]) -> {ok, pid()}.
start_link(ConnStr, Opts) ->
	gen_server:start_link(?MODULE, [ConnStr, Opts], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ConnStr, Opts]) ->
	?log_debug("init", []),
	{ok, ConnRef} = odbc:connect(ConnStr, Opts),

	{ok, #state{
		conn_ref = ConnRef
	}}.

handle_call({open, Table, _Opts}, _From, State = #state{
	conn_ref = ConnRef
}) ->
	Result =
		case odbc:describe_table(ConnRef, Table) of
			{ok, _Description} ->
				{ok, Table};
			{error, connection_closed} ->
				{error, connection_closed};
			{error, _Reason} ->
				CreateSql = lists:flatten(
					io_lib:format("CREATE TABLE ~s (k VARCHAR(~p), v VARCHAR(~p), PRIMARY KEY(k))",
					[Table, ?KLEN, ?VLEN])),
				{updated, _} = odbc:sql_query(ConnRef, CreateSql),
				{ok, Table}
		end,
	{reply, Result, State};

handle_call({read, Table, Key}, _From, State = #state{
	conn_ref = ConnRef
}) ->
	SelectSqlFmt = lists:flatten(
		io_lib:format("SELECT v FROM ~s WHERE k=?", [Table])),
	ParamKey = encode_key(Key),
	%?log_debug("~s (~p)", [SelectSqlFmt, ParamKey]),
	Result =
		case odbc:param_query(ConnRef, SelectSqlFmt, [ParamKey]) of
			{selected, ["v"], []} ->
				{error, no_entry};
			{selected, ["v"], [Value|_]} ->
				{ok, decode_value(Value)};
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Result, State};

handle_call({write, Table, Key, Value}, _From, State = #state{
	conn_ref = ConnRef
}) ->
	SelectSqlFmt = lists:flatten(
		io_lib:format("SELECT v FROM ~s WHERE k=?", [Table])),
	ParamKey = encode_key(Key),
	%?log_debug("~s (~p)", [SelectSqlFmt, ParamKey]),
	Result =
		case odbc:param_query(ConnRef, SelectSqlFmt, [ParamKey]) of
			{selected, ["v"], []} ->
				InsertSqlFmt = lists:flatten(
					io_lib:format("INSERT INTO ~s VALUES(?, ?)", [Table])),
				ParamKey = encode_key(Key),
				ParamValue = encode_value(Value),
				%?log_debug("~s (~p, ~p)", [InsertSqlFmt, ParamKey, ParamValue]),
				case odbc:param_query(ConnRef, InsertSqlFmt, [ParamKey, ParamValue]) of
					{updated, 1} ->
						ok;
					{error, Reason} ->
						{error, Reason}
				end;
			{selected, ["v"], _} ->
				UpdateSqlFmt = lists:flatten(
					io_lib:format("UPDATE ~s SET v=? WHERE k=?", [Table])),
				ParamValue = encode_value(Value),
				ParamKey = encode_key(Key),
				%?log_debug("~s (~p, ~p)", [UpdateSqlFmt, ParamValue, ParamKey]),
				case odbc:param_query(ConnRef, UpdateSqlFmt, [ParamValue, ParamKey]) of
					{updated, 0} ->
						ok;
					{updated, 1} ->
						ok;
					{error, Reason} ->
						{error, Reason}
				end;
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Result, State};

handle_call({delete, Table, Key}, _From, State = #state{
	conn_ref = ConnRef
}) ->
	DeleteSqlFmt = lists:flatten(
		io_lib:format("DELETE FROM ~s WHERE k=?", [Table])),
	ParamKey = encode_key(Key),
	%?log_debug("~s (~p)", [DeleteSqlFmt, ParamKey]),
	Result =
		case odbc:param_query(ConnRef, DeleteSqlFmt, [ParamKey]) of
			{updated, 0} ->
				{error, no_entry};
			{updated, 1} ->
				ok;
			{error, Reason} ->
				{error, Reason}
   		end,
	{reply, Result, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{
	conn_ref = ConnRef
}) ->
	ok = odbc:disconnect(ConnRef).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

encode_key(Key) ->
	encode("~p", ?KLEN, Key).

encode_value(Value) ->
	encode("~p", ?VLEN, Value).

encode(Fmt, Len, Value) ->
	Encoded = lists:flatten(io_lib:format(Fmt, [Value])),
	%?log_debug("~p", [length(Encoded)]),
	{{sql_varchar, Len}, [Encoded]}.

decode_value({Value}) ->
	{ok, Tokens, _} = erl_scan:string(Value ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
	Term.
