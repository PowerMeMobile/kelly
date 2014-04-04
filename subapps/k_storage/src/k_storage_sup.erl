-module(k_storage_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("k_common/include/logging.hrl").
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
    ?log_debug("init", []),
    {ok, {{one_for_all, 5, 10}, [
        {gen_storage_manager_sup, {gen_storage_manager_sup, start_link, []},
            permanent, 100000, supervisor, [gen_storage_manager_sup]},
        {gen_storage_manager, {gen_storage_manager, start_link, [k_storage_manager]},
            permanent, 10000, worker, [gen_storage_manager]}
    ]}}.
