-module(k_storage_sup).

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
			{k_storage_parts_sup,
				{k_storage_parts_sup, start_link, []}, permanent, 100000, supervisor, [k_storage_parts_sup]},
			{mongodb_storage,
				{mongodb_storage, start_link, []}, permanent, 100000, worker, [mongodb_storage]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
