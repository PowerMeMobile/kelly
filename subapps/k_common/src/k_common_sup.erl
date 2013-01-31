-module(k_common_sup).

-behaviour(supervisor).

-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-include("logging.hrl").
-include("supervisor_spec.hrl").

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
			{k_mnesia_schema,
				{k_mnesia_schema, start_link, []}, permanent, 100000, worker, [k_mnesia_schema]},
			{k_time_server,
				{k_time_server, start_link, []}, permanent, 1000, worker, [k_time_server]}
		]}
	}.
