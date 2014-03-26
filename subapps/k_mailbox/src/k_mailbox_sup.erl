%% @hidden

-module(k_mailbox_sup).

-behaviour(supervisor).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

-define(WORKER(Name),
	{Name, {Name, start_link, []}, permanent, 5000, worker, [Name]}).
-define(SUPERVISOR(Name),
	{Name, {Name, start_link, []}, permanent, infinity, supervisor, [Name]}).

%% API
-export([
	start_link/0
	]).

%% supervisor callbacks
-export([
	init/1
	]).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor Callbacks
%% ===================================================================

init([]) ->
	{ok, {

		{rest_for_one, 5, 10}, [

			?SUPERVISOR(k_mb_amqp_sup),
			?WORKER(k_mb_subscription_mgr),
			?WORKER(k_mb_postponed_queue),
			?WORKER(k_mb_wpool)
		]}
	}.
