-module(k_odbc_storage).

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

-record(state, {}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(Table::string(), Opts::[tuple()]) -> {ok, string()} | {error, Reason::term()}.
open(Table, Opts) ->
	odbc_do(
		fun(Pid) ->
			gen_server:call(Pid, {open, Table, Opts}, infinity)
		end).

-spec close(Table::string()) -> ok | {error, Reason::term()}.
close(_Table) ->
	ok.

-spec read(Table::string(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Table, Key) ->
	odbc_do(
		fun(Pid) ->
			gen_server:call(Pid, {read, Table, Key}, infinity)
		end).

-spec write(Table::string(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Table, Key, Value) ->
	odbc_do(
		fun(Pid) ->
			gen_server:call(Pid, {write, Table, Key, Value}, infinity)
		end).

-spec delete(Table::string(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Table, Key) ->
	odbc_do(
		fun(Pid) ->
			gen_server:call(Pid, {delete, Table, Key}, infinity)
		end).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	Args = application:get_all_env(?APP),
	?log_debug("starting pooler...", []),
	ok = init_pooler(Args),
	?log_debug("pooler started.", []),
	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok = application:stop(pooler).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec init_pooler(Args::[tuple()]) -> ok | {error, Reason::term()}.
init_pooler(Args) ->
	PoolMaxCount = proplists:get_value(pool_max_count, Args),
	PoolInitCount = proplists:get_value(pool_init_count, Args),
	ConnStr = proplists:get_value(connection_string, Args),
	?log_debug("~s", [ConnStr]),
	Opts = [{scrollable_cursor, off}],
	Pools = [[{name, "odbc-dyn"},
			  {max_count, PoolMaxCount},
			  {init_count, PoolInitCount},
			  {start_mfa, {k_odbc_connector, start_link, [ConnStr, Opts]}}
			]],
	application:set_env(pooler, pools, Pools),
	ensure_app_started(pooler).

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

odbc_do(Fun) ->
	case pooler:take_member() of
		error_no_members ->
			{error, no_storage};
		Pid ->
			try Fun(Pid) of
				{error, disconnected} ->
					pooler:return_member(Pid, fail),
					{error, no_storage};
				Result ->
					pooler:return_member(Pid, ok),
					Result
			catch
				_Exception ->
					pooler:return_member(Pid, fail),
					{error, no_storage}
					%{error, Exception}
			end
	end.
