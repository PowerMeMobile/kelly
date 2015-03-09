-module(k_defers_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

-define(WORKER(Name), {Name, {Name, start_link, []}, permanent, 5000, worker, [Name]}).

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
    {ok, {{one_for_one, 5, 10}, [
        ?WORKER(k_defers_publisher),
        ?WORKER(k_defers_timer)
    ]}}.
