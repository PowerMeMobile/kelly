-module(k_storage_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

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
		{one_for_all, 5, 10}, [
			{k_storage_manager_sup,
				{k_storage_manager_sup, start_link, []}, permanent, 100000, supervisor, [k_storage_manager_sup]},
			{k_time_server,
				{k_time_server, start_link, []}, permanent, 1000, worker, [k_time_server]},
			{k_storage_events_manager,
				{k_storage_events_manager, start_link, []}, permanent, 10000, worker, [k_storage_events_manager]},
			{k_storage_manager,
				{k_storage_manager, start_link, []}, permanent, 100000, worker, [k_storage_manager]}
		]}
	}.
