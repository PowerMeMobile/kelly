%% @TODO Add cron like functionality?
%% @TODO Compute timeout relatively to next checktime

-module(k_mb_gcollector).

-behaviour(gen_server).

-compile({no_auto_import, [now/0]}).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-record(state, {
    timeout :: integer()
}).

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	start_link/0,
    new_expire/0,
    is_expired/1
]).

%% ===================================================================
%% gen_server Function Exports
%% ===================================================================

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
    terminate/2,
	code_change/3
]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

%% @doc Starts garbage collector process.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns expired time for new item.
-spec new_expire() -> Time :: integer().
new_expire() ->
    now() + period().

%% @doc Inspects input time for expired.
-spec is_expired(ExpireTime :: integer()) -> boolean().
is_expired(ExpireTime) ->
    is_expired(ExpireTime, now()).

%% ===================================================================
%% GenServer Function Definitions
%% ===================================================================

init([]) ->
    Timeout = k_mb_config:get_env(purge_rate),
    purge(),
    {ok, #state{timeout = Timeout}, Timeout}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State = #state{timeout = Timeout}) ->
    purge(),
    {noreply, State, Timeout};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Function Definitions
%% ===================================================================

now() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

period() ->
    k_mb_config:get_env(expiration_date).

is_expired(ExpireTime, Now) when ExpireTime =< Now ->
    true;
is_expired(_ExpireTime, _Now) ->
    false.

purge() ->
    k_mb_db:delete_expired(now()).
