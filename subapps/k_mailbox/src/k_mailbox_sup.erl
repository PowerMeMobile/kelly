%% @hidden

-module(k_mailbox_sup).

-behaviour(supervisor).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

%% ===================================================================
%% Functions Exports
%% ===================================================================

%% API
-export([
	start_link/0
	]).

%% supervisor callbacks
-export([
	init/1
	]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor Functions Definitions
%% ===================================================================

init([]) ->
	?log_debug("initialization...", []),
	{ok, {

		{rest_for_one, 5, 10}, [

			{k_mb_map_mgr, {k_mb_map_mgr, start_link, []},
				permanent, 5000, worker, [k_mb_map_mgr]},

			{k_mb_postponed_queue, {k_mb_postponed_queue, start_link, []},
				permanent, 5000, worker, [k_mb_postponed_queue]},

			{k_mb_wpool, {k_mb_wpool, start_link, []},
				permanent, 5000, worker, [k_mb_wpool]},

			{k_mb_gcollector, {k_mb_gcollector, start_link, []},
				permanent, 5000, worker, [k_mb_gcollector]}
		]}
	}.