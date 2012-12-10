-module(k_aaa_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% Supervisor callbacks
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
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, {
		{one_for_one, 5, 10}, [
			{sid_to_cid,
				{kv_storage_common, start_link, [sid_to_cid]}, permanent, 1000000, worker, [kv_storage_common]}
			%% {customers,
			%% 	{kv_storage_common, start_link, [customers]}, permanent, 1000000, worker, [kv_storage_common]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================

