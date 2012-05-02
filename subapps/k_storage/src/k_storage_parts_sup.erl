-module(k_storage_parts_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

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
	?log_debug("init", []),
	{ok, {
		{one_for_one, 5, 10}, [
			{msg_info_id,
				{k_storage_parts_mgr, start_link, [msg_info]}, permanent, 10000, worker, [k_storage_parts_mgr]},
			{msg_status_id,
				{k_storage_parts_mgr, start_link, [msg_status]}, permanent, 10000, worker, [k_storage_parts_mgr]},
			{in_to_out_id,
				{k_storage_parts_mgr, start_link, [in_to_out]}, permanent, 10000, worker, [k_storage_parts_mgr]},
			{out_to_in_id,
				{k_storage_parts_mgr, start_link, [out_to_in]}, permanent, 10000, worker, [k_storage_parts_mgr]},
			{dlr_msg_status_id,
				{k_storage_parts_mgr, start_link, [dlr_msg_status]}, permanent, 10000, worker, [k_storage_parts_mgr]}
		]}
	}.
