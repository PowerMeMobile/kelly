-module(k_mailbox_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
	?log_debug("k_mailbox_sup: init", []),
	{ok, {
		%% As in k_handlers_sup we tolerate 2 amqp connection problems in 1 minute.
		%% Others are not expected to cause problems.
		{rest_for_one, 2, 1}, [
			{mailbox_mnesia_init, {k_mailbox_mnesia_init, start_link, []},
				transient, 10000, worker, [k_mailbox_mnesia_init]},

			%% It's impossible to tie an amqp connection to this sub-tree,
			%% because it already belongs to the amqp_client application.
			%% Instead the amqp connection is started and served in the
			%% k_mailbox_ampq_connection module.
			{mailbox_amqp_connection, {k_mailbox_amqp_connection, start_link, []},
				permanent, 10000, worker, [k_mailbox_amqp_connection]},

			{mailbox_worker_sup, {k_mailbox_worker_sup, start_link, []},
				permanent, infinity, supervisor, [k_mailbox_worker_sup]},

			{mailbox_worker_wakeup, {k_mailbox_worker_srv, wake_up_all, []},
				temporary, 1000, worker, [k_mailbox_worker_srv]}
		]}
	}.
