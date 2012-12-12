-module(k_storage_parts_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper
-define(CHILD(Name),
	{Name, {k_storage_parts_mgr, start_link, [Name]},
			permanent, 10000, worker, [k_storage_parts_mgr]}).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{one_for_one, 5, 10}, []}}.
