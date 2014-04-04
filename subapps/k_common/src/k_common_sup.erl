-module(k_common_sup).

-behaviour(supervisor).

-export([
    start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-include("logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?log_debug("init", []),
    {ok, {{one_for_one, 5, 10}, []}}.
