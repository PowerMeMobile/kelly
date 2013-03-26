-module(k_storage_manager).

%% API
-export([
	start_link/0,
	get_storage_mode/0,
	wait_for_static_storage/0
]).

%% !!! DO NOT USE DIRECTLY !!!
-export([
	notify_event/1
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

-type event_name() :: 'ResponseEndEvent' | 'DeliveryEndEvent' | 'ShiftEvent'.
-type storage_mode() :: 'Response' | 'Delivery' | 'Normal'.
-type reason() :: term().

-record(state, {
	storage_mode :: storage_mode()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_storage_mode() -> {ok, storage_mode()} | {error, reason()}.
get_storage_mode() ->
	gen_server:call(?MODULE, get_storage_mode, infinity).

-spec wait_for_static_storage() -> ok.
wait_for_static_storage() ->
	{_, _} = gproc:await({n, l, k_static_storage}),
	ok.

-spec notify_event(event_name()) -> ok.
notify_event(Event) ->
	gen_server:cast(?MODULE, {notify_event, Event}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	%% initialize static storage.
	gen_server:cast(self(), start_static_storage),
	{ok, #state{}}.

handle_call(get_storage_mode, _From, State = #state{
	storage_mode = StorageMode
}) ->
	Mode =
		case StorageMode of
			'Response' -> k_response_mode_storage;
			'Delivery' -> k_delivery_mode_storage;
			'Normal'   -> k_normal_mode_storage
		end,
	{reply, {ok, Mode}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(start_static_storage, State = #state{}) ->
	try start_static_storage() of
		ok ->
			%% initialize dynamic storage.
			gen_server:cast(self(), start_dynamic_storage),
			%% signal static storage is ready.
			gproc:reg({n, l, k_static_storage}),
			{noreply, State}
	catch
		Exc:Err ->
			%% signal static storage is ready even though it's not.
			%% the code that depends on this will crash shortly anyway.
			gproc:reg({n, l, k_static_storage}),
			{stop, {Exc, Err, erlang:get_stacktrace()}, State}
	end;

handle_cast(start_dynamic_storage, State = #state{}) ->
	{ok, StorageMode} = k_storage_events_manager:get_storage_mode(),
	ok = start_dynamic_storage(StorageMode),

	{noreply, State#state{storage_mode = StorageMode}};

handle_cast({notify_event, {CurrMode, Event, NextMode}}, State = #state{
	storage_mode = CurrMode
}) ->
	?log_info("~p -> ~p -> ~p", [CurrMode, Event, NextMode]),
	ok = handle_event(CurrMode, Event, NextMode),
	{noreply, State#state{
		storage_mode = NextMode
	}};

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

ensure_static_storage_index(ServerName) ->
	ok = mongodb_storage:ensure_index(ServerName, k1api_sms_request_id_to_msg_ids,
	{key, {customer_id, 1, user_id, 1, src_addr, 1, req_id, 1}}).

ensure_dynamic_storage_index(ServerName) ->
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {ri, 1, imi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {ci, 1, ct, 1, imi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {gi, 1, omi, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mt_messages,
		{key, {rqt, 1}}),
	ok = mongodb_storage:ensure_index(ServerName, mo_messages,
		{key, {rqt, 1}}).

start_static_storage() ->
	{ok, StaticProps} = application:get_env(?APP, static_storage),

	{ok, Pid} = k_storage_manager_sup:start_child([{server_name, k_static_storage} | StaticProps]),
	true = register(k_static_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, k_static_storage]),

	ok = ensure_static_storage_index(k_static_storage).

start_dynamic_storage('Normal') ->
	ok = start_curr_dynamic_storage();
start_dynamic_storage('Response') ->
	ok = start_curr_dynamic_storage(),
	ok = start_prev_dynamic_storage();
start_dynamic_storage('Delivery') ->
	ok = start_curr_dynamic_storage(),
	ok = start_prev_dynamic_storage().

start_curr_dynamic_storage() ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),

	{ok, ShiftDbName} = k_storage_events_manager:get_curr_shift_db_name(),

	%% start current dynamic storage.
	CurrProps = [{server_name, k_curr_dynamic_storage}, {mongodb_dbname, ShiftDbName} | DynamicProps],

	{ok, Pid} = k_storage_manager_sup:start_child(CurrProps),
	true = register(k_curr_dynamic_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, k_curr_dynamic_storage]),

	ok = ensure_dynamic_storage_index(k_curr_dynamic_storage).

start_prev_dynamic_storage() ->
	{ok, DynamicProps} = application:get_env(?APP, dynamic_storage),

	{ok, ShiftDbName} = k_storage_events_manager:get_prev_shift_db_name(),

	%% start previous dynamic storage.
	PrevProps = [{server_name, k_prev_dynamic_storage}, {mongodb_dbname, ShiftDbName} | DynamicProps],

	{ok, Pid} = k_storage_manager_sup:start_child(PrevProps),
	true = register(k_prev_dynamic_storage, Pid),
	?log_debug("~p registered as ~p", [Pid, k_prev_dynamic_storage]),

	ok = ensure_dynamic_storage_index(k_prev_dynamic_storage).

shift_storages() ->
	%% unregister and stop previous storage, if one.
	case whereis(k_prev_dynamic_storage) of
		undefined -> noop;
		PrevPid ->
			true = unregister(k_prev_dynamic_storage),
			ok = mongodb_storage:stop(PrevPid)
	end,
	%% turn the current storage into previous.
	CurrPid = whereis(k_curr_dynamic_storage),
	true = unregister(k_curr_dynamic_storage),
	true = register(k_prev_dynamic_storage, CurrPid),
	%% start new current storage.
	ok = start_curr_dynamic_storage().

handle_event('Normal', 'ShiftEvent', 'Response') ->
	ok = shift_storages();
handle_event('Response', 'ResponseEndEvent', 'Delivery') ->
	ok;
handle_event('Delivery', 'DeliveryEndEvent', 'Normal') ->
	%% even though it's possible and even logical to close
	%% the previous storage, it still makes sense not to,
	%% because there might be processes having links to it
	%% and we will get `noproc' exceptions'.
	ok.
