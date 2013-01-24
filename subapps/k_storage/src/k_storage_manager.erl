-module(k_storage_manager).

%% API
-export([
	start_link/0
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
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	%% start static storage.
	{ok, StaticProps} = application:get_env(?APP, static_storage),
	{ok, _} = k_storage_manager_sup:start_child([{server_name, k_static_storage} | StaticProps]),

	%% start dynamic storages.
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),
	DbNameFmt = proplists:get_value(mongodb_dbname_fmt, DynamicProps),

	{{CurYear, CurMonth, _}, _} = calendar:universal_time(),
	{PrevYear, PrevMonth, _} = edate:shift({CurYear, CurMonth, 1}, -1, month),

	%% build current and previous db names in format YYYY-MM.
	CurDateStr = lists:flatten(io_lib:format("~B_~2..0B", [CurYear, CurMonth])),
	PrevDateStr = lists:flatten(io_lib:format("~B_~2..0B", [PrevYear, PrevMonth])),

	CurDbName = list_to_binary(io_lib:format(DbNameFmt, [CurDateStr])),
	PrevDbName = list_to_binary(io_lib:format(DbNameFmt, [PrevDateStr])),

	%% start current dynamic storage.
	CurProps = [{server_name, k_current_dynamic_storage}, {mongodb_dbname, CurDbName} | DynamicProps],
	{ok, _} = k_storage_manager_sup:start_child(CurProps),

	%% start previous dynamic storage.
	PrevProps = [{server_name, k_previous_dynamic_storage}, {mongodb_db_name, PrevDbName} | DynamicProps],
	{ok, _} = k_storage_manager_sup:start_child(PrevProps),

	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

ensure_index() ->
	mongodb_storage:ensure_index(k_current_dynamic_storage, mt_messages,
		{key, {ci, 1, ct, 1, imi, 1}, unique, true, dropDups, true}),
	mongodb_storage:ensure_index(k_current_dynamic_storage, mt_messages,
		{key, {qi, 1, omi, 1}}),
	mongodb_storage:ensure_index(k_current_dynamic_storage, mt_messages,
		{key, {rqt, 1}}),
	mongodb_storage:ensure_index(k_current_dynamic_storage, mo_messages,
		{key, {rqt, 1}}).
