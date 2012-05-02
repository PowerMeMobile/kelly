-module(k_config_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include("application.hrl").
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
			{gateways,
				{k_gen_storage_common, start_link, [gateways]}, permanent, 1000000, worker, [k_gen_storage_common]},
			{providers,
				{k_gen_storage_common, start_link, [providers]}, permanent, 1000000, worker, [k_gen_storage_common]},
			{networks,
				{k_gen_storage_common, start_link, [networks]}, permanent, 1000000, worker, [k_gen_storage_common]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
