-module(k_riak_storage).

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

-record(state, {}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(CollectionName::term(), Opts::[tuple()]) -> {ok, Bucket::binary()} | {error, Reason::term()}.
open(CollectionName, _Opts) ->
	{ok, term_to_binary(CollectionName)}.

-spec close(Bucket::binary()) -> ok | {error, Reason::term()}.
close(_Bucket) ->
	ok.

-spec read(Bucket::binary()) -> {ok, Value::term()} | {error, Reason::term()}.
read(Bucket) ->
	riak_do(
		fun(Pid) ->
			case riakc_pb_socket:list_keys(Pid, Bucket) of
				{ok, KeyBins} ->
					lists:foldl(
						fun(_, {error, Reason}) -> {error, Reason};
						   (KeyBin, {ok, Acc}) ->
								case riakc_pb_socket:get(Pid, Bucket, KeyBin) of
									{ok, Object} ->
										ValueBin = riakc_obj:get_value(Object),
										Key = binary_to_term(KeyBin),
										Value = binary_to_term(ValueBin),
										{ok, [{Key, Value} | Acc]};
									{error, Reason} ->
										{error, Reason}
								end
						end,
						{ok, []},
						KeyBins);
				Other ->
					Other
			end
		end).

-spec read(Bucket::binary(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Bucket, Key) ->
	riak_do(
		fun(Pid) ->
			KeyBin = term_to_binary(Key),
			case riakc_pb_socket:get(Pid, Bucket, KeyBin) of
				{ok, Object} ->
					ValueBin = riakc_obj:get_value(Object),
					Value = binary_to_term(ValueBin),
					{ok, Value};
				{error, notfound} ->
					{error, no_entry};
				Other ->
					Other
			end
		end).

-spec write(Bucket::binary(), Key::term(), Key::term()) -> ok | {error, Reason::term()}.
write(Bucket, Key, Value) ->
	riak_do(
		fun(Pid) ->
			KeyBin = term_to_binary(Key),
			ValueBin = term_to_binary(Value),
			Object = riakc_obj:new(Bucket, KeyBin, ValueBin),
			riakc_pb_socket:put(Pid, Object)
		end).

-spec delete(Bucket::binary(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Bucket, Key) ->
	riak_do(
		fun(Pid) ->
			KeyBin = term_to_binary(Key),
			case riakc_pb_socket:delete(Pid, Bucket, KeyBin) of
			   ok ->
					ok;
			   {error, notfound} ->
					{error, no_entry};
			   Other ->
					Other
			end
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

terminate(_Reason, _State) ->
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
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	Autoreconnect = proplists:get_value(auto_reconnect, Args),
	Pools = [[{name, "haproxy-dyn"},
			  {max_count, PoolMaxCount},
			  {init_count, PoolInitCount},
			  {start_mfa, {riakc_pb_socket, start_link, [Host, Port, [{auto_reconnect, Autoreconnect}]]}}
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

riak_do(Fun) ->
	riak_do(Fun, 1).

riak_do(_, 0) ->
	{error, no_storage};
riak_do(Fun, N) ->
	case pooler:take_member() of
		error_no_members ->
			{error, no_storage};
		Pid ->
			try Fun(Pid) of
				{error, disconnected} ->
					pooler:return_member(Pid, fail),
					riak_do(Fun, N-1);
				Result ->
					pooler:return_member(Pid, ok),
					Result
			catch
				_Exception ->
					pooler:return_member(Pid, fail),
					riak_do(Fun, N-1)
					%{error, Exception}
			end
	end.

%% http://paste.lisp.org/display/127952,1/raw
