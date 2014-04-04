-module(k_gen_consumer).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    start_link/3,
    requeue_message/1,
    subscribe/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include("amqp_req.hrl").

-record(state, {
        amqp_conn,
        amqp_chan,
        amqp_xchg,
        qname,
        qtag,
        ref_dict,
        pid_dict,
        module,
        mod_state
}).

%% ===================================================================
%% Callbacks
%% ===================================================================

-callback init(Args :: term()) ->
    {ok, ModState :: term()} |
    {stop, StopReason :: term()} |
    ignore.

-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-callback handle_message(k_amqp_req:req(), State :: term()) ->
    {noreply, {Pid :: pid(), MonRef :: reference()}, NewState :: term()} |
    {noreply, NewState :: term()}.

-callback handle_subscribe(State :: term()) ->
    {ok, QoS :: pos_integer(), QueueName :: binary(), DeclareQueueFlag :: boolean(),
        XChangeName :: binary(), NewState :: term()} |
    {ok, QoS :: pos_integer(), QueueName :: binary(), DeclareQueueFlag :: boolean(),
        NewState :: term()}.

-callback handle_consume_ok(State :: term()) -> {noreply, NewState :: term()}.

-callback handle_cancel_ok(State :: term()) ->
    {noreply, Reason :: term(), NewState :: term()}.

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link({local, atom()}, atom(), [any()]) -> {ok, pid()}.
start_link({local, Name}, Module, Args) ->
    {ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, {Module, Args}, []),
    subscribe(Pid),
    {ok, Pid}.

-spec start_link(atom(), [any()]) -> {ok, pid()}.
start_link(Module, Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Module, Args}, []),
    subscribe(Pid),
    {ok, Pid}.

-spec requeue_message(pid()) -> ok.
requeue_message(Pid) ->
    ok = gen_server:call(Pid, requeue_message).

-spec subscribe(pid()) -> ok.
subscribe(Pid) ->
    gen_server:cast(Pid, do_subscribe).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init({ConsumerModule, ExtraParams}) ->
    case ConsumerModule:init(ExtraParams) of
        {ok, MState} ->
            {ok, #state{module = ConsumerModule, mod_state = MState}};
        {stop, Reason} ->
            {stop, Reason};
        ignore ->
            ignore
    end.

handle_call(requeue_message, {WPid, _}, State = #state{
    ref_dict = RefDict,
    pid_dict = PidDict,
    amqp_chan = Channel
}) ->
    MonitorRef = dict:fetch(WPid, PidDict),
    NewPidDict = dict:erase(WPid, PidDict),
    {Tag, WPid} = dict:fetch(MonitorRef, RefDict),
    rmql:basic_reject(Channel, Tag, true),
    NewRefDict = dict:erase(MonitorRef, RefDict),
    true = erlang:demonitor(MonitorRef, [flush]),
    {reply, ok, State#state{ref_dict = NewRefDict, pid_dict = NewPidDict}};

handle_call(Msg, From, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    case Mod:handle_call(Msg, From, MState) of
        {reply, Reply, NewMState} ->
            {reply, Reply, State#state{mod_state = NewMState}};
        {reply, Reply, NewMState, hibernate} ->
            {reply, Reply, State#state{mod_state = NewMState}, hibernate};
        {reply, Reply, NewMState, Timeout} ->
            {reply, Reply, State#state{mod_state = NewMState}, Timeout};
        {noreply, NewMState} ->
            {noreply, State#state{mod_state = NewMState}};
        {noreply, NewMState, hibernate} ->
            {noreply, State#state{mod_state = NewMState}, hibernate};
        {stop, Reason, Reply, NewMState} ->
            {stop, Reason, Reply, State#state{mod_state = NewMState}};
        {stop, Reason, NewMState} ->
            {stop, Reason, State#state{mod_state = NewMState}}
    end.

handle_cast(do_subscribe, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    {ok, QoS, QName, DeclareQueue, BXchg, NewMState} =
    case Mod:handle_subscribe(MState) of
        {ok, Opts, Q, DQ, Xchg, S} -> {ok, Opts, Q, DQ, Xchg, S};
        {ok, Opts, Q, DQ, S} -> {ok, Opts, Q, DQ, undefined, S}
    end,
    {ok, Connection} = rmql:connection_start(),
    {ok, Channel} = rmql:channel_open(Connection),
    declare_exchange(Channel, BXchg),
    declare_queue(Channel, QName, DeclareQueue),
    queue_bind(Channel, QName, BXchg),
    ok = rmql:basic_qos(Channel, QoS),
    case rmql:basic_consume(Channel, QName, false) of
        {ok, QTag} ->
            link(Channel),
            Dict = dict:new(),
            {noreply, State#state{
                        amqp_chan = Channel,
                        amqp_xchg = BXchg,
                        qname = QName,
                        qtag = QTag,
                        ref_dict = Dict,
                        pid_dict = Dict,
                        mod_state = NewMState}};
        {error,{{shutdown,{server_initiated_close,404, _Description}},_}} ->
            ?log_warn("Queue [~p] not found. Retry after 5 sec.", [QName]),
            {ok, _TRef} = timer:apply_after(5000, ?MODULE, subscribe, [self()]),
            rmql:channel_close(Channel),
            rmql:connection_close(Connection),
            {noreply, State}
    end;

handle_cast(Msg, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    case Mod:handle_cast(Msg, MState) of
        {noreply, NewMState} ->
            {noreply, State#state{mod_state = NewMState}};
        {noreply, NewMState, hibernate} ->
            {noreply, State#state{mod_state = NewMState}, hibernate};
        {noreply, NewMState, Timeout} ->
            {noreply, State#state{mod_state = NewMState}, Timeout};
        {stop, Reason, NewMState} ->
            {stop, Reason, State#state{mod_state = NewMState}}
    end.

handle_info(#'basic.consume_ok'{}, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    ?log_debug("got basic.consume_ok", []),
    {noreply, NewMState} = Mod:handle_consume_ok(MState),
    {noreply, State#state{mod_state = NewMState}};

handle_info(#'basic.cancel_ok'{}, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    ?log_debug("got basic.cancel_ok - quitting...", []),
    {noreply, Reason, NewMState} =
        Mod:handle_cancel_ok(MState),
    {stop, Reason, State#state{mod_state = NewMState}};

handle_info({#'basic.deliver'{delivery_tag = Tag},
            #amqp_msg{ props = #'P_basic'{
                content_type = ContentType,
                reply_to = ReplyTo
            }, payload = Payload}},
            State = #state{
                amqp_chan = Channel,
                ref_dict = RefDict,
                pid_dict = PidDict,
                mod_state = ModState,
                module = Mod}) ->
    Req = #amqp_req{
        'req.payload' = Payload,
        'req.p_basic.content_type' = ContentType,
        'req.p_basic.reply_to' = ReplyTo,
        'amqp.channel' = Channel
    },
    {{ProcPid, ProcMonRef}, NewState} =
        case Mod:handle_message(Req, ModState) of
            {noreply, {Pid, MonRef}, NewMState} ->
                {{Pid, MonRef}, NewMState};
            {noreply, NewMState} ->
                {undefined, NewMState}
        end,
    {ok, NewRefDict, NewPidDict} = register(RefDict, PidDict, {ProcPid, ProcMonRef}, Tag),

    {noreply, State#state{
        ref_dict = NewRefDict,
        pid_dict = NewPidDict,
        mod_state = NewState
    }};

handle_info({'DOWN', MonitorRef, _Type, _Object, normal}, State = #state{
    ref_dict = RefDict,
    pid_dict = PidDict,
    module = _Mod,
    amqp_chan = Channel
}) ->
    {DTag, WPid} = dict:fetch(MonitorRef, RefDict),
    NewRefDict = dict:erase(MonitorRef, RefDict),
    ok = rmql:basic_ack(Channel, DTag),
    NewPidDict = dict:erase(WPid, PidDict),
    {noreply, State#state{ref_dict = NewRefDict, pid_dict = NewPidDict}};

handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, State = #state{
    module = Mod,
    ref_dict = RefDict,
    pid_dict = PidDict,
    amqp_chan = Channel
}) ->
    {Tag, WPid} = dict:fetch(MonitorRef, RefDict),
    NewRefDict = dict:erase(MonitorRef, RefDict),
    rmql:basic_reject(Channel, Tag, true),
    ?log_debug("worker[~p] -> Rejected: ~p", [Mod, Info]),
    NewPidDict = dict:erase(WPid, PidDict),
    {noreply, State#state{ref_dict = NewRefDict, pid_dict = NewPidDict}};

handle_info(Info, State = #state{
    mod_state = MState,
    module = Mod
}) ->
    case Mod:handle_info(Info, MState) of
        {noreply, NewMState} ->
            {noreply, State#state{mod_state = NewMState}};
        {noreply,NewMState,hibernate} ->
            {noreply, State#state{mod_state = NewMState}, hibernate};
        {noreply, NewMState,Timeout} ->
            {noreply, State#state{mod_state = NewMState}, Timeout};
        {stop, Reason, NewMState} ->
            {stop, {error, Reason}, State#state{mod_state = NewMState}}
    end.

terminate(Reason, #state{
    mod_state = MState,
    module = Mod
}) ->
    Mod:terminate(Reason, MState).

code_change(OldVsn, State = #state{
    mod_state = MState,
    module = Mod
}, Extra) ->
    case Mod:code_change(OldVsn, MState, Extra) of
        {ok, NewMState} ->
            {ok, State#state{mod_state = NewMState}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Local functions
%% ===================================================================

declare_queue(_Channel, QName, false) ->
    ?log_notice("Abort to declare queue [~p]: false flag", [QName]),
    ok;
declare_queue(Channel, QName, DeclareQueue) when DeclareQueue == undefined orelse
                                                 DeclareQueue == true ->
    ?log_debug("Try to declare queue [~p]", [QName]),
    Durable = true,
    Exclusive = false, % default
    AutoDelete = false, % default
    ok = rmql:queue_declare(Channel, QName, Durable, Exclusive, AutoDelete),
    ?log_info("Queue [~p] was successfully declared", [QName]).

declare_exchange(_Channel, undefined) ->
    ok;
declare_exchange(Channel, Xchg) ->
    ok = rmql:exchange_declare(Channel, Xchg, fanout, true),
    ?log_info("Xchg[~p] declare OK", [Xchg]).

queue_bind(_Channel, _QName, undefined) ->
    ok;
queue_bind(Channel, QName, Xchg) ->
    ok = rmql:queue_bind(Channel, QName, Xchg),
    ?log_info("queue[~p] binding OK", [QName]).

register(RefDict, PidDict, {Pid, MonRef}, Tag) ->
    NewRefDict = dict:store(MonRef, {Tag, Pid}, RefDict),
    NewPidDict = dict:store(Pid, MonRef, PidDict),
    {ok, NewRefDict, NewPidDict}.
