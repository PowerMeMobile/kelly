-module(k_amqp_gen_consumer).

-behaviour(k_gen_consumer).

%% API
-export([
	start_link/1
]).

%% Callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3,

	handle_subscribe/1,
	handle_message/4,
	handle_consume_ok/1,
	handle_cancel_ok/1
]).

-include_lib("k_common/include/logging.hrl").
-include("gen_consumer_spec.hrl").

-record(state, {
	request :: atom(),
	handler :: atom()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(Request::atom()) -> {ok, pid()}.
start_link(Request) ->
	k_gen_consumer:start_link({local, Request}, ?MODULE, [Request]).

%% ===================================================================
%% k_gen_consumer callbacks
%% ===================================================================

init([Request])->
	?log_debug("init(~p)...", [Request]),
	{ok, #state{request = Request}}.

handle_subscribe(State = #state{
	request = Request
}) ->
	?log_debug("handling subscribe(~p)...", [Request]),
	{ok, Args} = application:get_env(Request),
	Queue = proplists:get_value(queue, Args),
	Handler = proplists:get_value(handler, Args),
	Options = proplists:get_value(options, Args),
	{ok, Options, Queue, State#state{handler = Handler}}.

handle_message(ContentType, Message, Channel, State = #state{
	handler = Handler
}) ->
	%?log_debug("got message: ~p", [Message]),
	{ok, Pid} = k_worker_sup:process(Handler, ContentType, Message, Channel),
	{noreply, Pid, State}.

handle_consume_ok(State = #state{}) ->
	?log_debug("got basic.consume_ok", []),
	{noreply, State}.

handle_cancel_ok(State = #state{}) ->
	?log_debug("got basic.cancel_ok - quitting...", []),
	{stop, got_cancel, State}.

handle_call(_Request, _From, State = #state{}) ->
 	{stop, bad_call_request, State}.

handle_cast(_Msg, State = #state{}) ->
	{stop, bad_cast_request, State}.

handle_info(_Info, State = #state{}) ->
	{stop, bad_info_request, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
	{ok, State}.
