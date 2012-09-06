-module(k_handlers_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
    {ok, {
		%% Below are 6 amqp consumers, each of which may terminate due to an amqp connection problem.
		%% The MaxR = 12 and MaxT = 1 values mean that we tolerate 2 amqp connection problems
		%% in each of them in 1 minute. Adjust the values to more appropriate ones if you need.
		{one_for_one, 12, 1}, [
			{k_amqp_bind_request,
				{k_amqp_gen_consumer, start_link, [k_amqp_bind_request]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_k1api_auth_handler,
				{k_amqp_gen_consumer, start_link, [k_k1api_auth_handler]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_amqp_sms_request,
				{k_amqp_gen_consumer, start_link, [k_amqp_sms_request]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_amqp_sms_response,
				{k_amqp_gen_consumer, start_link, [k_amqp_sms_response]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_amqp_receipt_batch,
				{k_amqp_gen_consumer, start_link, [k_amqp_receipt_batch]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_amqp_funnel_events,
				{k_amqp_gen_consumer, start_link, [k_amqp_funnel_events]}, permanent, 10000, worker, [k_amqp_gen_consumer]},
			{k_amqp_incoming_sms,
				{k_amqp_gen_consumer, start_link, [k_amqp_incoming_sms]}, permanent, 10000, worker, [k_amqp_gen_consumer]},

			%%% NOTE: k_worker_sup MUST be at the end of SPEC %%%
			{k_worker_sup,
				{k_worker_sup, start_link, []}, permanent, infinity, supervisor, [k_worker_sup]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
