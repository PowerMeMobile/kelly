-module(k_mailbox_worker_sup).

-behaviour(supervisor).

-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("k_mailbox_sup: init", []),
	{ok, {
		{simple_one_for_one, 5, 10}, [ %%% What if a worker fails?!
			{mailbox_worker, {k_mailbox_worker_srv, start_link, []},
				 temporary, 1000000, worker, [k_mailbox_worker_srv]}
		]}
	}.

%%% Internal

