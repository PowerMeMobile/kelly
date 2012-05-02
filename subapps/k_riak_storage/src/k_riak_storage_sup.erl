-module(k_riak_storage_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, {
		{one_for_one, 5, 10}, [
			{k_riak_storage,
				{k_riak_storage, start_link, []}, permanent, 1000000, worker, [k_riak_storage]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
