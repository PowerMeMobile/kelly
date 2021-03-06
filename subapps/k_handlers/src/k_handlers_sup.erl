-module(k_handlers_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-define(HANDLER(Name),
    {Name, {k_amqp_gen_consumer, start_link, [Name]},
        permanent, 10000, worker, [k_amqp_gen_consumer]
    }
).
-define(WORKER(Name), {Name, {Name, start_link, []}, permanent, 5000, worker, [Name]}).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

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
    %% Below are amqp consumers, each of which may terminate due to an amqp connection problem.
    %% The MaxR = 12 and MaxT = 1 values mean that we tolerate 2 amqp connection problems
    %% in each of them in 1 minute. Adjust the values to more appropriate ones if you need.
    {ok, {{one_for_one, 12, 1}, [
        ?WORKER(k_handlers_auth_cache),
        ?WORKER(k_event_manager),

        ?WORKER(k_amqp_api_handler),
        ?WORKER(k_amqp_auth_handler),

        ?HANDLER(amqp_sms_request),
        ?HANDLER(amqp_funnel_sms_request),
        ?HANDLER(amqp_sms_request_deferred),

        ?HANDLER(amqp_sms_response),
        ?HANDLER(amqp_receipt_batch),
        ?HANDLER(amqp_funnel_events),
        ?HANDLER(amqp_incoming_sms),

        %%% NOTE: k_worker_sup MUST be at the end of SPEC %%%
        {k_worker_sup,
            {k_worker_sup, start_link, []}, permanent, infinity, supervisor, [k_worker_sup]}
    ]}}.

%% ===================================================================
%% Internal
%% ===================================================================
