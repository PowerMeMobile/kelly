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
    handle_message/2,
    handle_consume_ok/1,
    handle_cancel_ok/1
]).

-include("gen_consumer_spec.hrl").
-include_lib("alley_common/include/logging.hrl").

-record(state, {
    handler_spec_name :: atom(),
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

init([HandlerSpecName])->
    ?log_debug("Initialize amqp handler: ~p", [HandlerSpecName]),
    {ok, #state{handler_spec_name = HandlerSpecName}}.

handle_subscribe(State = #state{handler_spec_name = HandlerSpecName}) ->
    ?log_debug("Handling subscribe [amqp handler: ~p]...", [HandlerSpecName]),
    {ok, Props} = application:get_env(HandlerSpecName),
    Queue = proplists:get_value(queue, Props),
    Handler = proplists:get_value(handler, Props),
    QoS = proplists:get_value(rmq_qos, Props),
    DeclareQueue = proplists:get_value(declare_queue, Props),
    ?log_debug("Handler [~p] will declare queue [~p]: ~p", [Handler, Queue, DeclareQueue]),
    {ok, QoS, Queue, DeclareQueue, State#state{handler = Handler}}.

handle_message(Req, State = #state{
    handler = Handler
}) ->
    %?log_debug("got message: ~p", [Message]),
    {ok, {Pid, MonRef}} = k_worker_sup:process(Handler, Req),
    {noreply, {Pid, MonRef}, State}.

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
