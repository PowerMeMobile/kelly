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
			{k_uuid, {k_uuid, start_link, []}, permanent, 5000, worker, [k_uuid]},
			{k_mnesia_schema, {k_mnesia_schema, start_link, []}, permanent, 100000, worker, [k_mnesia_schema]}
		]}
	}.
