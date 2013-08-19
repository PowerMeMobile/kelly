-module(k_handlers_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-define(HANDLER(Name),
	{Name,
		{k_amqp_gen_consumer, start_link, [Name]},
		permanent, 10000, worker, [k_amqp_gen_consumer]}).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_common/include/supervisor_spec.hrl").

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
			?HANDLER(k_amqp_bind_request),
			?HANDLER(k_k1api_auth_handler),
			?HANDLER(k_k1api_delivery_status_req_handler),
			?HANDLER(k_k1api_retrieve_sms_req_handler),
			?HANDLER(k_k1api_subscribe_handler),
			?HANDLER(k_amqp_k1api_sms_request),
			?HANDLER(k_amqp_sms_request),
			?HANDLER(k_amqp_sms_response),
			?HANDLER(k_amqp_receipt_batch),
			?HANDLER(k_amqp_funnel_events),
			?HANDLER(k_amqp_incoming_sms),
			%%% NOTE: k_worker_sup MUST be at the end of SPEC %%%
			{k_worker_sup,
				{k_worker_sup, start_link, []}, permanent, infinity, supervisor, [k_worker_sup]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
