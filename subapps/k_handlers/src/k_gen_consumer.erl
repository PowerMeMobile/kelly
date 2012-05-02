-module(k_gen_consumer).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([behaviour_info/1]).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-record(state, {
		amqp_conn,
		amqp_chan,
		amqp_xchg,
		qname,
		qtag,
		dict,
		module,
		mod_state
}).

-spec behaviour_info(callbacks) -> [ {atom(), integer()} ].
behaviour_info(callbacks) ->
	[{init, 1},
	{handle_call, 3},
	{handle_cast, 2},
	{handle_info, 2},
	{terminate, 2},
	{code_change, 3},
%% gen_consumer behaviour callbacks
	{handle_message, 4},
	{handle_subscribe, 1},
	{handle_consume_ok, 1},
	{handle_cancel_ok, 1}];
behaviour_info(_) ->
	undefined.

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link( {local, atom()}, atom(), [any()] ) -> {ok, pid()}.
start_link({local, Name}, Module, Args) ->
	{ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, {Module, Args}, []),
	gen_server:cast(Pid, do_subscribe),
	{ok, Pid}.

-spec start_link( atom(), [any()] ) -> {ok, pid()}.
start_link(Module, Args) ->
	{ok, Pid} = gen_server:start_link(?MODULE, {Module, Args}, []),
	gen_server:cast(Pid, do_subscribe),
	{ok, Pid}.

% call(Pid, Msg) ->
% 	gen_server:call(Pid, {consumer_call, Msg}, infinity).

% ack(Tag)->
% 	gen_server:cast(?MODULE, {ack, Tag}),
% 	ok.

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
	?log_debug("subscribing...", []),

	{ok, AmqpOpts, QName, BXchg, NewMState} =
	case Mod:handle_subscribe(MState) of
		{ok, Opts, Q, Xchg, S} -> {ok, Opts, Q, Xchg, S};
		{ok, Opts, Q, S} -> {ok, Opts, Q, undefined, S}
	end,

	{ok, AmqpSpec, Qos} = parse_opts(AmqpOpts),

	{ok, Connection} = amqp_connection:start(AmqpSpec),

	true = link(Connection),

	?log_debug("successfully connected to AMQP: ~p", [Connection]),
	{ok, Channel} = amqp_connection:open_channel(Connection),

%% for bublishing test messages

			%%% FOR EXCHANGE %%%
	% Publish = #'basic.publish'{exchange = <<"funnel.sms_request">>},

			%%% FOR QUEUE %%%
	% Publish = #'basic.publish'{routing_key = <<"pmm.funnel.bind_requests">>},

	% Msg = #amqp_msg{ payload = <<"Hello world!">> },
	% amqp_channel:cast(Channel, Publish, Msg),
	% ?log_debug("HALT", []),
	% halt(),

	declare_exchange(Channel, BXchg),

	declare_queue(Channel, QName),

	queue_bind(Channel, QName, BXchg),

	#'basic.qos_ok'{} = amqp_channel:call(Channel,#'basic.qos'{prefetch_count =Qos}),
	?log_debug("QOS: ~p", [Qos]),

	Sub = #'basic.consume'{ queue = QName },
	#'basic.consume_ok'{consumer_tag = QTag } = amqp_channel:subscribe(Channel, Sub, self()),

	Dict = dict:new(),

	{noreply, State#state{
		amqp_conn = Connection,
		amqp_chan = Channel,
		amqp_xchg = BXchg,
		qname = QName,
		qtag = QTag,
		dict = Dict,
		mod_state = NewMState}};

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
				content_type = ContentType
			}, payload = Content}},
			State = #state{
				amqp_chan = Channel,
				dict = Dict,
				mod_state = ModState,
				module = Mod}) ->
	% ?log_debug("got message: ~p", [Content]),
	{ProcPid, NewState} =
	case Mod:handle_message(ContentType, Content, Channel, ModState) of
		{noreply, Pid, NewMState} ->
			{Pid, NewMState};
		{noreply, NewMState} ->
			{undefined, NewMState}
	end,
	{ok, NewDict} = register(Dict, ProcPid, Tag),

	{noreply, State#state{
		dict = NewDict,
		mod_state = 	NewState
		}};

handle_info({'DOWN', MonitorRef, _Type, _Object, normal}, State = #state{
	dict = Dict,
	module = _Mod,
	amqp_chan = Channel
}) ->
	Tag = dict:fetch(MonitorRef, Dict),
	NewDict = dict:erase(MonitorRef, Dict),
	Ack = #'basic.ack'{delivery_tag = Tag},
	amqp_channel:cast(Channel, Ack),
	% ?log_debug("worker[~p] -> Acked.", [_Mod]),
	{noreply, State#state{dict = NewDict}};

handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, State = #state{
	module = Mod,
	dict = Dict,
	amqp_chan = Channel
}) ->
	Tag = dict:fetch(MonitorRef, Dict),
	NewDict = dict:erase(MonitorRef, Dict),
	Requeue = #'basic.reject'{delivery_tag = Tag, requeue = true},
	ok = amqp_channel:call(Channel, Requeue),
	?log_debug("worker[~p] -> Rejected: ~p", [Mod, Info]),
	{noreply, State#state{dict = NewDict}};

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
	ok = Mod:terminate(Reason, MState).

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

declare_queue(Channel, QName) ->
	?log_debug("declaring queue[~p]", [QName]),
	QDeclare = #'queue.declare'{ queue = QName, durable = true },
	#'queue.declare_ok'{} = amqp_channel:call(Channel, QDeclare),
	?log_info("queue[~p] declare OK", [QName]).

declare_exchange(_Channel, undefined) ->
	ok;
declare_exchange(Channel, Xchg) ->
	XDeclare = #'exchange.declare'{ exchange = Xchg,
									durable = true,
									type = <<"fanout">> },
	?log_debug("declaring exchange[~p]", [Xchg]),
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, XDeclare),
	?log_info("Xchg[~p] declare OK", [Xchg]).

queue_bind(_Channel, _QName, undefined) ->
	ok;
queue_bind(Channel, QName, Xchg) ->
	?log_info("binding queue.. ", []),
	QBind = #'queue.bind'{ queue = QName, exchange = Xchg },
	#'queue.bind_ok'{} = amqp_channel:call(Channel, QBind),
	?log_info("queue[~p] binding OK", [QName]).

register(Dict, undefined, _Tag) -> 	%% must save Tag (may be reject mes
									%% here?)
	{ok, Dict};
register(Dict, Pid, Tag) ->
	MonRef = erlang:monitor(process, Pid),
	NewDict = dict:store(MonRef, Tag, Dict),
	{ok, NewDict}.

parse_opts(Opts) ->
	Host    = proplists:get_value(host, Opts, "localhost"),
	Port 	= proplists:get_value(port, Opts, 5672),
	Xchg 	= proplists:get_value(exchange, Opts, <<"/">>),
	User 	= proplists:get_value(username, Opts, <<"guest">>),
	Pass 	= proplists:get_value(password, Opts, <<"guest">>),
	Qos 	= proplists:get_value(qos, Opts, 0),

	AmqpSpec = #amqp_params_network{
					username = User,
					password = Pass,
					virtual_host = Xchg,
					host = Host,
					port = Port,
					heartbeat = 1
				},
	{ok, AmqpSpec, Qos}.
