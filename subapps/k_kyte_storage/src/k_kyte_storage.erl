-module(k_kyte_storage).

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
-include_lib("kyte/include/kyte.hrl").

-record(state, {
	pool :: pid()
}).

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(FileName::string(), Opts::[tuple()]) -> {ok, pid()} | {error, Reason::term()}.
open(FileName, Opts) ->
	Pool = gen_server:call(?MODULE, get_pool, infinity),
	Args = build_db_args(FileName, Opts),
	kyte:db_open(Pool, Args).

-spec close(Db::pid()) -> ok | {error, Reason::term()}.
close(Db) ->
	kyte:db_close(Db).

-spec read(Db::pid(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Db, Key) ->
	case kyte:db_get(Db, Key) of
		{error, "no record"} ->
			{error, no_entry};
		Other ->
			Other
	end.

-spec write(Db::pid(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Db, Key, Value) ->
	kyte:db_set(Db, Key, Value).

-spec delete(Db::pid(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Db, Key) ->
	case kyte:db_del(Db, Key) of
		{error, "no record"} ->
			{error, no_entry};
		Other ->
			Other
	end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	Args = application:get_all_env(?APP),
	PoolSize = proplists:get_value(pool_size, Args),
	{ok, Pool} = kyte:pool_create(PoolSize),

	{ok, #state{
		pool = Pool
	}}.

handle_call(get_pool, _From, State = #state{
	pool = Pool
}) ->
	{reply, Pool, State};

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

build_db_args(FileName, Opts) ->
	DataRoot = proplists:get_value(data_root, Opts, "data"),
	NameSuffix = proplists:get_value(name_suffix, Opts, ".kch"),
	Parts = case proplists:get_value(parts, Opts, single) of
				single ->
					single;
				PartsCount when is_integer(PartsCount) ->
					kyte:parts_post_hash_sha(PartsCount)
			end,
	KeyCodec = proplists:get_value(key_codec, Opts, etf),
	ValCodec = proplists:get_value(val_codec, Opts, etf),
	#kyte_db_args{
		file = DataRoot ++ "/" ++ FileName ++ postfix_fmt(Parts) ++ NameSuffix,
		parts = Parts,
		key_codec = KeyCodec,
		val_codec = ValCodec
	 }.

postfix_fmt(single) ->
	"";
postfix_fmt(_) ->
	"_~p".
