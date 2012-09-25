%% @private

-module(k_mb_wpool).

-behaviour(gen_wp).

-include_lib("k_common/include/logging.hrl").
-include_lib("gen_wp/include/gen_wp_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include("subscription.hrl").
-include("pending_item.hrl").

-record(state, {
}).

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	start_link/0,
	process_incoming_item/1
	]).

%% ===================================================================
%% GenWp Callback Functions Exports
%% ===================================================================

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2,

	handle_fork_cast/3,
	handle_fork_call/4,
	handle_child_forked/3,
	handle_child_terminated/4
	]).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	WPSize = k_mb_config:get_env(pool_size),
	gen_wp:start_link({local, ?MODULE}, ?MODULE, [], [{pool_size, WPSize}]).

-spec process_incoming_item(Item :: #k_mb_pending_item{}) -> ok.
process_incoming_item(Item = #k_mb_pending_item{}) ->
	gen_wp:cast(?MODULE, {process_item, Item}).

%% ===================================================================
%% GenWp Callback Functions Definitions
%% ===================================================================

init([]) ->
	?log_debug("started", []),
	{ok, Items} = k_mb_db:get_items(),
	Process = fun(Item) ->
			case Item#k_mb_pending_item.item_id of
				wait_for_sub ->
					k_mb_db:set_pending(Item#k_mb_pending_item.item_id);
				_ -> ok
			end,
			k_mb_wpool:process_incoming_item(Item)
		end,
	lists:foreach(Process, Items),
	{ok, #state{}}.


handle_call(Request, _ReplyTo, State) ->
	{stop, {badarg, Request}, badarg, State}.

handle_cast({process_item, Item}, State = #state{}) ->
	{fork, {process_item, Item}, State};

handle_cast(Message, State) ->
	{stop, {badarg, Message}, State}.

handle_info(Info, State) ->
	{stop, {badarg, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

handle_fork_call( _Arg, _CallMess, _ReplyTo, _WP ) ->
	?log_error("bad call request", []),
	{noreply, bad_request}.

handle_fork_cast(_Arg, {process_item, Item = #k_mb_pending_item{}}, _WP) ->
	#k_mb_pending_item{
		item_id = ItemID,
		expire = Expire
		} = Item,

	case k_mb_gcollector:is_expired(Expire) of
		true ->
			k_mb_db:delete_items([ItemID]),
			?log_debug("Pending item [~p] expired. Deleted.", [ItemID]);
		_ -> process(Item)
	end,
	{noreply, normal};

handle_fork_cast(_Arg, _CastMess, _WP) ->
	?log_error("bad cast request", []),
	{noreply, bad_request}.

handle_child_forked(_Task, _Child, State = #state{}) ->
	{noreply, State#state{}}.

handle_child_terminated(normal, _Task, _Child, State = #state{}) ->
	%?log_debug("successful", []),
	{noreply, State#state{}};

handle_child_terminated(Reason, _Task = {process_item, Item}, _Child, State = #state{}) ->
	case k_mb_postponed_queue:postpone(Item#k_mb_pending_item{error = Reason}) of
		{postponed, Seconds} ->
			?log_error("Item processing terminated. It will be resend "
			"after ~p sec. Reason: ~p", [Seconds, Reason]),
			{noreply, State#state{}};
		{error, rich_max} ->
			?log_error("Item rich max number of attempts. Discard message. Reason: ~p",
			[Reason]),
			{noreply, State#state{}};
		Error ->
			?log_error("Got unexpected error: ~p. Terminating.",
			[Error]),
			{stop, {unexpected_case_clause, Error}}
	end.


%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

process(Item) ->
	#k_mb_pending_item{
		item_id = ItemID,
		customer_id = CustomerID,
		user_id = UserID,
		content_type = ContentType
		} = Item,
	case k_mb_map_mgr:get_subscription(CustomerID, UserID, ContentType) of
		{ok, SubscriptionID} ->
			?log_debug("Found suitable subscription [~p]", [SubscriptionID]),
			send_item(Item, SubscriptionID);
		{error, no_subscription} ->
			?log_debug("Suitable subscription NOT FOUND. Waiting for suitable subscription", []),
			k_mb_db:set_wait(ItemID)
	end.

send_item(Item, SubID) ->
	#k_mb_pending_item{
		item_id = ItemID,
		sender_addr = SenderAddr,
		dest_addr = DestAddr,
		message_body = Message,
		encoding = Encoding,
		content_type = ContentType
		} = Item,
	Binary = build_funnel_incoming_sms_dto(ItemID, SenderAddr, DestAddr, Message, Encoding),
	QName = list_to_binary(io_lib:format("pmm.funnel.nodes.~s", [uuid:to_string(SubID)])),
	?log_debug("Send item to funnel queue [~p]", [QName]),
	Result = k_mb_amqp_producer_srv:send(ItemID, Binary, QName, ContentType),
	case Result of
		{ok, delivered} ->
			?log_debug("Successfully delivered [item:~p]", [ItemID]),
			k_mb_db:delete_items([ItemID]);
		{error, timeout} ->
			postpone_item(Item, timeout)
	end.

postpone_item(Item, Error) ->
	case k_mb_postponed_queue:postpone(Item#k_mb_pending_item{error = Error}) of
		{postponed, Seconds} ->
			?log_error("Item processing terminated. It will be resend "
			"after ~p sec. Last error: ~p", [Seconds, Error]);
		{error, rich_max} ->
			?log_error("Item rich max number of attempts. "
			"Discard message. Last error: ~p", [Error]);
		Error ->
			?log_error("Got unexpected error: ~p. Terminating.",
			[Error])
	end.


build_funnel_incoming_sms_dto(ID, SenderAddr = #addr{}, DestAddr, Message, Encoding) ->
	SenderAddrDTO = #addr_dto{
		addr = SenderAddr#addr.addr,
		ton = SenderAddr#addr.ton,
		npi = SenderAddr#addr.npi
	},
	build_funnel_incoming_sms_dto(ID, SenderAddrDTO, DestAddr, Message, Encoding);
build_funnel_incoming_sms_dto(ID, SenderAddr, DestAddr = #addr{}, Message, Encoding) ->
	DestAddrDTO = #addr_dto{
		addr = DestAddr#addr.addr,
		ton = DestAddr#addr.ton,
		npi = DestAddr#addr.npi
	},
	build_funnel_incoming_sms_dto(ID, SenderAddr, DestAddrDTO, Message, Encoding);
build_funnel_incoming_sms_dto(BatchId, SenderAddr, DestAddr, Message, Encoding) ->
	Msg = #funnel_incoming_sms_message_dto{
		source = SenderAddr,
		dest = DestAddr,
		data_coding = Encoding,
		message = Message
	},
	Batch = #funnel_incoming_sms_dto{
		id = BatchId,
		messages = [Msg]
	},
	{ok, Binary} = adto:encode(Batch),
	Binary.
