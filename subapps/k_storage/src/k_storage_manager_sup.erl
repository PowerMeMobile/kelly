-module(k_storage_manager_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_child/1
]).

%% supervisor callbacks
-export([init/1]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

-type reason() :: term().
-type plist() :: [{atom(), term()}].

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, reason()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(plist()) -> {ok, pid()} | {error, reason()}.
start_child(Props) ->
	{SupPid, _Value} = gproc:await({n, l, ?MODULE}),
	supervisor:start_child(SupPid, [Props]).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	gproc:reg({n, l, ?MODULE}),
	{ok, {{simple_one_for_one, 5, 10}, [
		{mongodb_storage,
			{mongodb_storage, start_link, []}, transient, 10000, worker, [mongodb_storage]}
	]}}.
