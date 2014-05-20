-module(k_mb_amqp_sup).

-behaviour(supervisor).

-include_lib("alley_common/include/logging.hrl").

%% API
-export([
    start_link/0
]).

%% Supervisor callbacks
-export([
    init/1
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> ignore.
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {k_mb_amqp_consumer_srv, {k_mb_amqp_consumer_srv, start_link, []},
            permanent, 5000, worker, [k_mb_amqp_consumer_srv]},

        {k_mb_amqp_producer_srv, {k_mb_amqp_producer_srv, start_link, []},
            permanent, 5000, worker, [k_mb_amqp_producer_srv]}
    ]}}.
