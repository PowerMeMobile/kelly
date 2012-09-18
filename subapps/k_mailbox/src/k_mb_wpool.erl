%% @private

-module(k_mb_wpool).

-behaviour(gen_wp).

-include_lib("k_common/include/logging.hrl").
-include_lib("gen_wp/include/gen_wp_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").
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
	%?log_debug("handle process item", []),
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
	?log_error("error", []),
	k_mb_postponed_queue:postpone(Item#k_mb_pending_item{error = Reason}),
	{noreply, State#state{}}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

process(Item) ->
	%?log_debug("process", []),
	#k_mb_pending_item{
		item_id = ItemID,
		customer_id = CustomerID,
		user_id = UserID,
		content_type = ContentType
		} = Item,
	case k_mb_map_mgr:get_subscription(CustomerID, UserID, ContentType) of
		{ok, SubscriptionID} ->
			?log_debug("Found suitable subscription [~p]", [SubscriptionID]),
			process(Item, SubscriptionID);
		{error, no_subscription} ->
			?log_debug("Suitable subscription NOT FOUND", []),
			k_mb_db:set_wait(ItemID)
	end.

process(Item, SubID) ->
	#k_mb_pending_item{
		item_id = ItemID
		} = Item,

	Timeout = k_mb_config:get_env(request_timeout),

	QName = list_to_binary(io_lib:format("pmm.funnel.nodes.~s", [uuid:to_string(SubID)])),
	?log_debug("Send item to funnel queue [~p]", [QName]),
	ok = k_mb_amqp_producer_srv:send(QName, Item, Timeout),
	k_mb_db:delete_items([ItemID]).
