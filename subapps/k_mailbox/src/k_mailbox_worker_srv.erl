-module(k_mailbox_worker_srv).

-behaviour(gen_server).

-export([
	start_link/1,
	wake_up_all/0
]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-include_lib("stdlib/include/qlc.hrl").
-include_lib("k_common/include/FunnelAsn.hrl").

-include("application.hrl").
-include("connection.hrl").
-include("pending_item.hrl").

-define(SUBMIT_ITEMS_CHUNK, 100).

-record(state, {
	customer_id,
	consumers = [],
	connections = [],
	postponed_connections = [],
	outgoing_requests = [],

	amqp_chan :: pid(),
	reply_qname :: binary(),
	reply_qtag :: binary(),
	reply_ctag :: binary(),

	no_ack_timeout :: integer(),
	restore_connection_timeout :: integer()
}).

-spec start_link(CustomerID :: string()) -> {ok, pid()}.
start_link(CustomerID) ->
	gen_server:start_link(?MODULE, {CustomerID}, []).

wake_up_all() ->
	gen_server:start_link(?MODULE, wake_up_all, []).

init(wake_up_all) ->
	?log_debug("init(~p)", [wake_up_all]),
	QH = qlc:q([
		Item#k_mb_pending_item.customer_id
		|| Item <- mnesia:table(k_mb_pending_item)],
		{unique, true}),
	{atomic, CIDs} = mnesia:transaction(fun() ->
		qlc:e(QH)
	end),
	[k_mailbox:get_customer_worker(CID)
		|| CID <- CIDs],
	{ok, wake_up_all};

init({CustomerID}) ->
	case (
		try
			gproc:add_local_name({?MODULE, CustomerID})
		catch
			error:badarg ->
				false
		end
	) of
		true ->
			put(p, lists:flatten(io_lib:format("mb_worker[cust:~p]:", [CustomerID]))),
			?log_debug("~s started", [get(p)]),

			{ok, NoAckTimeout} = application:get_env(?APP, no_ack_timeout),
			{ok, RestoreConnectionTimeout} = application:get_env(?APP, restore_connection_timeout),
			{ok, AMQP_Chan} = start_amqp_chan(),
			{ok, QName, QTag} = declare_reply_queue(AMQP_Chan, CustomerID),

			{ok, Connections} = select_connections(CustomerID),
			?log_debug("~s Saved connections: ~p", [get(p), Connections]),

			{ok, State1} = register_multiple_connections(Connections, #state{
				customer_id = CustomerID,
				reply_qname = QName,
				reply_qtag = QTag,
				amqp_chan = AMQP_Chan,
				no_ack_timeout = NoAckTimeout,
				restore_connection_timeout = RestoreConnectionTimeout
			}),
			{ok, State2} = rebuild_consumers(State1),

			Delta = content_type_delta([], State1 #state.consumers),
			cast_rescan_pending_items(Delta),

			{ok, State2};
		false ->
			{stop, normal}
	end.

handle_call({register_connection, ConnID, ConnType}, _From, State = #state{}) ->
	?log_debug("~s Trying to register connection ~p/~p", [get(p), ConnID, ConnType]),
	try
		{ok, State1} = register_connection(ConnID, ConnType, State),
		{ok, State2} = rebuild_consumers(State1),
		Delta = content_type_delta(State #state.consumers, State2 #state.consumers),

		?log_debug("~s Added [~p/~p]", [get(p), ConnType, ConnID]),
		ok = cast_rescan_pending_items(Delta),
		{reply, ok, State2}
	catch error:{badmatch, Error} ->
		{reply, Error, State}
	end;

handle_call({unregister_connection, ConnID}, _From, State = #state{}) ->
	?log_debug("~s Trying to unregister connection ~p", [get(p), ConnID]),
	try
		{ok, State1} = unregister_connection(ConnID, State),
		{ok, State2} = rebuild_consumers(State1),
		{reply, ok, State2}
	catch error:{badmatch, Error} ->
		{reply, Error, State}
	end;

handle_call(dump_state, _From, State) ->
	?log_debug("~s dumping state: ~p", [get(p), State]),
	{reply, State, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({rescan_pending_items, []}, State) -> {noreply, State};
handle_cast({rescan_pending_items, Delta}, State = #state{
		customer_id = CustID
	}) ->
	% TODO: make Funnel emit the UpEvent only when the node-queue is ready
	{ok, DelayMS} = application:get_env(?APP, delay_rescan_pendings),
	ok = timer:sleep(DelayMS),
	ok = foreach_pending_item(CustID, Delta),
	{noreply, State};

handle_cast({submit_item, Item}, State = #state{}) ->
	ItemID = Item#k_mb_pending_item.item_id,
	?log_debug("~s Got notification on [item:~p]", [get(p), ItemID]),
	try
		{ok, NState} = process_submit_item(Item, State),
		{noreply, NState}
	catch error:{badmatch, Error} ->
		?log_error("~s Failed to handle notification of [item:~p]: ~p", [get(p), ItemID, Error]),
		{ok, State}
	end;

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({#'basic.return'{
			reply_code = AmqpCode,
			reply_text = <<"NO_ROUTE">> = AmqpText,
			exchange = AmqpXchg,
			routing_key = AmqpQName
		},
		#amqp_msg{props = #'P_basic'{correlation_id = ItemIDBin}}},
		State = #state{}) ->
	ItemID = binary_to_list(ItemIDBin),
	?log_error("~s Temporarily failed to submit [item:~p] - ~p/~p [~p, ~p] - probably node has gone.",
			[get(p), ItemID, AmqpCode, AmqpText, AmqpXchg, AmqpQName]),

	%% the connection seems to be down.
	%% mark the item as `pending'. it will be resubmitted later.
	{ok, NState} = handle_submit_item_failure(ItemID, pending, false, State),
	{noreply, NState};

handle_info({#'basic.return'{
			reply_code = AmqpCode,
			reply_text = AmqpText,
			exchange = AmqpXchg,
			routing_key = AmqpQName
		},
		#amqp_msg{props = #'P_basic'{correlation_id = ItemIDBin}}},
		State = #state{}) ->
	ItemID = binary_to_list(ItemIDBin),
	?log_error("~s Failed to submit [item:~p] - ~p/~p [~p, ~p]",
			[get(p), ItemID, AmqpCode, AmqpText, AmqpXchg, AmqpQName]),

	%% there was an error while performing the request.
	%% store failure reason to item's state.
	{ok, NState} = handle_submit_item_failure(
			ItemID,
			{failed, amqp, {AmqpCode, AmqpText, AmqpXchg, AmqpQName}},
			false,
			State),
	{noreply, NState};

handle_info({#'basic.deliver'{
			delivery_tag = DTag,
			consumer_tag = CTag
		},
		#amqp_msg{props = #'P_basic'{content_type = <<"BatchAck">>}, payload = Content}},
		State = #state{
			reply_ctag = CTag,
			amqp_chan = AMQP_Chan}) ->
	?log_debug("~s Got from funnel: 'BatchAck' / ~p", [get(p), Content]),
	amqp_channel:cast(AMQP_Chan, #'basic.ack'{delivery_tag = DTag}),
	case 'FunnelAsn':decode('BatchAck', Content) of
		{ok, #'BatchAck'{
			batchId = ItemID
		}} ->
			?log_debug("~s Successfully delivered [item:~p]", [get(p), ItemID]),
			{ok, NState} = handle_submit_item_success(ItemID, State),
			{noreply, NState};
		{error, AsnErr} ->
			?log_error("~s Failed to decode 'BatchAck' due to ~p : ~p", [get(p), AsnErr, Content]),
			{noreply, State}
	end;

handle_info(#'basic.consume_ok'{
		consumer_tag = CTag
	}, State = #state{}) ->
	{noreply, State #state{reply_ctag = CTag}};

handle_info({no_ack_timer, ItemID}, State = #state{}) ->
	?log_error("~s Fun-Node didn't acknowledge the submit [item:~p]", [get(p), ItemID]),

	%% mark the item as `pending'. it will be resubmitted later.
	%% consider this connection to be broken.
	%% delete both the broken connection and the request.
	{ok, NState} = handle_submit_item_failure(ItemID, pending, true, State),
	{noreply, NState};

handle_info({restore_connection_timer, ConnID}, State = #state{}) ->
	?log_error("~s Trying to restore connection ~p", [get(p), ConnID]),

	%% restore the postponed connection.
	case restore_connection(ConnID, State) of
		{ok, State1} ->
			{ok, State2} = rebuild_consumers(State1),
			Delta = content_type_delta(State#state.consumers, State2#state.consumers),
			ok = cast_rescan_pending_items(Delta),
			{noreply, State2};
		_Error ->
			{noreply, State}
	end;

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Internal

%% Control
cast_rescan_pending_items(Delta) ->
	gen_server:cast(self(), {rescan_pending_items, Delta}).

cast_submit_item(Item) ->
	gen_server:cast(self(), {submit_item, Item}).

%% %%%%%%%%%%%%

update_item_state(ItemID, NState) ->
	{atomic, UpdResult} = mnesia:transaction(fun() ->
		case mnesia:read({k_mb_pending_item, ItemID}) of
			[] -> no_entry;
			[Item] -> mnesia:write(Item#k_mb_pending_item{state = NState})
		end
	end),
	?log_debug("~s Updating [item:~p] state to ~p: ~p", [get(p), ItemID, NState, UpdResult]),
	ok.

foreach_pending_item(CustID, Delta) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		Query = qlc:q([
				Item ||
				Item <- mnesia:table(k_mb_pending_item),
					Item#k_mb_pending_item.customer_id == CustID
					andalso
					Item#k_mb_pending_item.state == pending
					andalso
					lists:any(fun(Ct) ->
						Ct == Item#k_mb_pending_item.content_type
					end, Delta)
		]),
		Cursor = qlc:cursor(Query),
		case iterate_through_cursor(Cursor, ?SUBMIT_ITEMS_CHUNK) of
			no_more_items ->
				ok;
			more_items ->
				cast_rescan_pending_items(Delta),
				ok
		end,
		qlc:delete_cursor(Cursor)
	end),
	Result.

iterate_through_cursor(_Cursor, 0) ->
	more_items;
iterate_through_cursor(Cursor, N) ->
	case qlc:next_answers(Cursor, 1) of
		[] ->
			no_more_items;
		[Item] ->
			cast_submit_item(Item),
			iterate_through_cursor(Cursor, N-1)
	end.

process_submit_item(Item = #k_mb_pending_item{}, State = #state{}) ->
	ItemID = Item#k_mb_pending_item.item_id,
	ContentType = Item#k_mb_pending_item.content_type,

	Consumers = State#state.consumers,
	AMQP_Chan = State#state.amqp_chan,
	ReplyQName = State#state.reply_qname,
	OutgoingRequests = State#state.outgoing_requests,
	NoAckTimeout = State#state.no_ack_timeout,

	case connection_id_for_content_type(ContentType, Consumers) of
		{ok, ConnID} ->
			QName = list_to_binary(io_lib:format("pmm.funnel.nodes.~s", [ConnID])),
			ok = submit_item_to_amqp(Item, QName, ReplyQName, AMQP_Chan),
			{ok, TRef} = timer:send_after(NoAckTimeout, {no_ack_timer, ItemID}),
			{ok, State#state{
				%% Affiliate ItemID to the particular Fun-Node here.
				outgoing_requests = [{ItemID, {ConnID, TRef}} | OutgoingRequests]
			}};
		skip ->
			{ok, State}
	end.

connection_id_for_content_type(ContentType, Consumers)
	when ContentType == <<"OutgoingBatch">>
	orelse ContentType == <<"ReceiptBatch">>
->
	case proplists:get_value(ContentType, Consumers, undefined) of
		undefined ->
			skip;
		ConnID ->
			{ok, ConnID}
	end.

submit_item_to_amqp(#k_mb_pending_item{
				item_id = ItemID,
				content_type = CT,
				content_body = Payload
			}, QName, ReplyQName, AMQP_Chan) ->
	Pub = #'basic.publish'{
		routing_key = QName, exchange = <<"">>,
		mandatory = true,
		immediate = false
	},
	Props = #'P_basic'{correlation_id = list_to_binary(ItemID), content_type = CT, reply_to = ReplyQName},
	Msg = #amqp_msg{props = Props, payload = Payload},
	%% submit the item.
	ok = amqp_channel:call(AMQP_Chan, Pub, Msg),
	%% mark the item as `submitted'.
	ok = update_item_state(ItemID, submitted).

start_amqp_chan() ->
	{ok, AMQP_Conn} = k_mailbox_amqp_connection:get_connection(),
	{ok, AMQP_Chan} = amqp_connection:open_channel(AMQP_Conn),
	amqp_channel:register_return_handler(AMQP_Chan, self()),
	#'basic.qos_ok'{} = amqp_channel:call(AMQP_Chan, #'basic.qos'{prefetch_count = 100}),
	{ok, AMQP_Chan}.

declare_reply_queue(AMQP_Chan, CustomerID) ->
	QName = list_to_binary(io_lib:format("pmm.kelly.mailbox.~s", [CustomerID])),
	QDeclare = #'queue.declare'{
		queue = QName,
		durable = false,
		exclusive = true,
		auto_delete = true
	},
	#'queue.declare_ok'{} = amqp_channel:call(AMQP_Chan, QDeclare),
	#'basic.consume_ok'{consumer_tag = QTag} = amqp_channel:subscribe(
		AMQP_Chan,
		#'basic.consume'{queue = QName},
		self()
	),
	{ok, QName, QTag}.

%% %%%%%%%%%%%%%%%

%% Mnesia routines
select_connections(CustomerID) ->
	{atomic, Connections} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([
				{Conn#k_mb_connection.connection_id, Conn#k_mb_connection.connection_type}  ||
				Conn <- mnesia:table(k_mb_connection),
				Conn#k_mb_connection.customer_id == CustomerID
		]))
	end),
	{ok, Connections}.

%% %%%%%%%%%%%%%%%

%% State changing operations


register_multiple_connections(Connections, State) ->
	NewState = lists:foldl(fun({ConnID, ConnType}, OState = #state{}) ->
		{ok, NState} = register_connection(ConnID, ConnType, OState),
		NState
	end, State, Connections),
	{ok, NewState}.

register_connection(ConnID, ConnType, State = #state{
	connections = Connections,
	customer_id = CustID
}) ->
	case proplists:get_value(ConnID, Connections, undefined) of
		undefined ->
			ok = store_connection(CustID, ConnID, ConnType),
			{ok, State #state{
				connections = [{ConnID, ConnType} | Connections]
			}};
		_ ->
			{{error, already_registered}, State}
	end.

unregister_connection(ConnID, State = #state{
	connections = Connections,
	postponed_connections = PostponedConnections
}) ->
	case proplists:get_value(ConnID, Connections, undefined) of
		undefined ->
			 case proplists:get_value(ConnID, PostponedConnections, undefined) of
				undefined ->
					 {error, no_connection};
				_ ->
					{ok, State#state{
						postponed_connections = proplists:delete(ConnID, PostponedConnections)
					}}
			 end;
		_ ->
			forget_connection(ConnID),
			{ok, State#state{
				connections = proplists:delete(ConnID, Connections)
			}}
	end.

postpone_connection(ConnID, State = #state{
	connections = Connections,
	postponed_connections = PostponedConnections
}) ->
	case proplists:get_value(ConnID, Connections, undefined) of
		undefined ->
			{error, no_connection};
		Value ->
			{ok, State#state{
				connections = proplists:delete(ConnID, Connections),
				postponed_connections = [{ConnID, Value} | PostponedConnections]
			}}
	end.

restore_connection(ConnID, State = #state{
	connections = Connections,
	postponed_connections = PostponedConnections
}) ->
	case proplists:get_value(ConnID, PostponedConnections, undefined) of
		undefined ->
			{error, no_connection};
		Value ->
			{ok, State#state{
				connections = [{ConnID, Value} | Connections],
				postponed_connections = proplists:delete(ConnID, PostponedConnections)
			}}
	end.

rebuild_consumers(State = #state{
	connections = Connections
}) ->
	Consumers = perform_rebuild_consumers(content_types(), Connections, []),
	{ok, State #state{
		consumers = Consumers
	}}.

handle_submit_item_success(ItemID, State = #state{}) ->
	{atomic, _Result} = mnesia:transaction(fun() ->
		mnesia:delete({k_mb_pending_item, ItemID})
	end),
	OutgoingRequests = State#state.outgoing_requests,
	case proplists:get_value(ItemID, OutgoingRequests) of
		undefined ->
			{ok, State};
		{_ConnID, TRef} ->
			{ok, cancel} = timer:cancel(TRef),
			{ok, State#state{
				outgoing_requests = proplists:delete(ItemID, OutgoingRequests)
			}}
	end.

handle_submit_item_failure(ItemID, NewItemStatus, ReconnectLater, State = #state{}) ->
	OutgoingRequests = State#state.outgoing_requests,
	case proplists:get_value(ItemID, OutgoingRequests) of
		undefined ->
			{ok, State};
		{ConnID, _} ->
			?log_debug("~s Canceling outgoing requests for connection ~p", [get(p), ConnID]),
			{ok, RemainingRequests} =
				cancel_outgoing_requests_for_connection_id(OutgoingRequests, ConnID, NewItemStatus),
			State1 = State#state{outgoing_requests = RemainingRequests},
			case ReconnectLater of
				true ->
					postpone_and_reschedule_restore_connection(ConnID, State1);
				false ->
					delete_connection(ConnID, State1)
			end
	end.

delete_connection(ConnID, State1 = #state{}) ->
	?log_debug("~s Deleting connection ~p", [get(p), ConnID]),
	case unregister_connection(ConnID, State1) of
		{ok, State2} ->
			rebuild_consumers(State2);
		_Error ->
			{ok, State1}
	end.

postpone_and_reschedule_restore_connection(ConnID, State1 = #state{}) ->
	?log_debug("~s Postponing connection ~p", [get(p), ConnID]),
	case postpone_connection(ConnID, State1) of
		{ok, State2} ->
			case rebuild_consumers(State2) of
				{ok, State3} ->
					?log_debug("~s Scheduling restore connection ~p", [get(p), ConnID]),
					{ok, _State4} = schedule_restore_connection(ConnID, State3);
				_Error ->
					{ok, State2}
			end;
		_Error ->
			{ok, State1}
	end.

cancel_outgoing_requests_for_connection_id(OutgoingRequests, ConnIDToCancel, NewItemStatus) ->
	%% split outgoing requests to two parts: to cancel and remaining.
	{ToCancel, Remaining} = lists:foldl(
		fun(Req = {_, {ConnID, _}}, {ToCancelAcc, RemainingAcc}) ->
			case ConnID =:= ConnIDToCancel of
				true ->
					{[Req | ToCancelAcc], RemainingAcc};
				false ->
					{ToCancelAcc, [Req | RemainingAcc]}
			end
		end,
		{[], []}, OutgoingRequests),

	%% cancel requests.
	lists:foreach(
		fun({ItemID, {_, TRef}}) ->
			ok = update_item_state(ItemID, NewItemStatus),
			{ok, cancel} = timer:cancel(TRef)
		end,
		ToCancel),

	{ok, Remaining}.

schedule_restore_connection(ConnID, State = #state{
	restore_connection_timeout = RestoreConnectionTimeout
}) ->
	{ok, _TRef} = timer:send_after(RestoreConnectionTimeout, {restore_connection_timer, ConnID}),
	{ok, State}.

% Connection type filtering

-spec content_types() -> [content_type()].
content_types() -> [
		<<"OutgoingBatch">>,
		<<"ReceiptBatch">>
	].

-spec can_handle(ConnType :: connection_type(), ContentType :: content_type()) -> boolean().
can_handle('smpp.transceiver', ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle('smpp.receiver', ContentType)
	when (
			ContentType == <<"OutgoingBatch">> orelse
		    ContentType == <<"ReceiptBatch">>
	) -> true ;
can_handle(_ConnectionType, _ContentType) -> false.

-spec perform_rebuild_consumers(
	ContentTypes :: [content_type()],
	Connections :: [{connection_id(), connection_type()}],
	Map :: [{content_type(), connection_id()}]
) -> [{content_type() , connection_id()}].

perform_rebuild_consumers([], _, Map) -> Map;
perform_rebuild_consumers([ContentType | ContentTypesSoFar], Connections, Map) ->
	NMap = case get_suitable_connection(ContentType, Connections) of
		undefined ->
			Map;
		ConnID ->
			[{ContentType, ConnID} | Map]
	end,
	perform_rebuild_consumers(ContentTypesSoFar, Connections, NMap).

-spec get_suitable_connection(
	content_type(),
	[{connection_id(), connection_type()}]
) -> undefined | connection_id().

get_suitable_connection(_ContentType, []) -> undefined;
get_suitable_connection(ContentType, [{ConnID, ConnType} | ConnsSoFar]) ->
	case can_handle(ConnType, ContentType) of
		true -> ConnID;
		false -> get_suitable_connection(ContentType, ConnsSoFar)
	end.

% Storing routines
store_connection(CustID, ConnID, ConnType) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		ok = mnesia:write(#k_mb_connection{
			customer_id = CustID,
			connection_id = ConnID,
			connection_type = ConnType
		})
	end),
	ok.

forget_connection(ConnID) ->
	{atomic, Smth} = mnesia:transaction(fun() ->
		mnesia:delete({k_mb_connection, ConnID})
	end),
	Smth.

content_type_delta(Consumers, NConsumers) ->
	content_type_delta(Consumers, NConsumers, []).

content_type_delta(_Consumers, [], Delta) -> Delta;
content_type_delta(Consumers, [{NewCt, _} | NSoFar], Delta) ->
	case proplists:get_value(NewCt, Consumers, undefined) of
		undefined -> content_type_delta(Consumers, NSoFar, [NewCt | Delta]);
		_ -> content_type_delta(Consumers, NSoFar, Delta)
	end.
