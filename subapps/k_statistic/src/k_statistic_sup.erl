-module(k_statistic_sup).

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
		{one_for_one, 5, 10}, [
			{k_statistic_msg_stats,
				{k_statistic_msg_stats, start_link, []},
					permanent, 1000000, worker, [k_statistic_msg_stats]},
			{k_statistic_status_stats,
				{k_statistic_status_stats, start_link, []},
					permanent, 1000000, worker, [k_statistic_status_stats]},
			{k_statistic_incoming_msg_stats,
				{k_statistic_incoming_msg_stats, start_link, []},
					permanent, 1000000, worker, [k_statistic_incoming_msg_stats]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
