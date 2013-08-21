-module(k_worker).

-behaviour(gen_server).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(_Args) ->
	% ?log_debug("initialization...", []),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{stop, bad_call_request, State}.

handle_cast({process, {Module, Req, ConsumerPid}}, State = #state{}) ->
	% ?log_debug("got message: ~p", [Message]),
	case Module:process(Req) of
		{ok, List} when is_list(List) ->
			send_response(Req, List),
			{stop, normal, State};
		{error, not_enough_data_to_proceed} ->
			?log_debug("Not enough data to process receipt. Requeue.", []),
			k_gen_consumer:requeue_message(ConsumerPid),
			{stop, normal, State};
		{error, {database_connection_failure, Reason}} ->
			?log_debug("Database connection failure: ~p. Requeue.", [Reason]),
			k_gen_consumer:requeue_message(ConsumerPid),
			{stop, normal, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;

handle_cast(_Msg, State) ->
	{stop, bad_cast_request, State}.

handle_info(_Info, State) ->
	{stop, bad_info_request, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

send_response(_Req, []) ->
	ok;
send_response(Req, [Head|Tail]) ->
	{ok, Channel} = k_amqp_req:channel(Req),
	#worker_reply{
		reply_to = QName,
		payload = Mes,
		content_type = Type
	} = Head,
	{ok, Payload} = convert(Mes),
	Publish = #'basic.publish'{routing_key = QName},
	Props = #'P_basic'{content_type = Type},
	Msg = #amqp_msg{props = Props, payload = Payload},
	amqp_channel:call(Channel, Publish, Msg),
	send_response(Channel, Tail).

convert(Mes) when is_list(Mes) ->
	{ok, list_to_binary(Mes)};
convert(Mes) when is_binary(Mes) ->
	{ok, Mes}.
