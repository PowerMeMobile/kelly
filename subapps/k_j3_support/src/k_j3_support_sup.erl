-module(k_j3_support_sup).

-behaviour(supervisor).

-export([
    start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?log_debug("k_j3_support_sup: init", []),
    {ok, {{rest_for_one, 5, 10}, [
        {k_snmp_tasks_queue, {k_snmp_tasks_queue, start_link, []}, permanent, 1000000, worker, [k_snmp_tasks_queue]},
        {k_snmp, {k_snmp, start_link, []}, permanent, 1000000, worker, [k_snmp]}
    ]}}.
