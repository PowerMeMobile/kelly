-module(k_support_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

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
    {ok, {{one_for_one, 5, 10}, [
        {k_support_snmp, {k_support_snmp, start_link, []}, permanent, 5000, worker, [k_support_snmp]}
    ]}}.
