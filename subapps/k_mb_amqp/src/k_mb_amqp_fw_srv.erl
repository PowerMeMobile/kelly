-module(k_mb_amqp_fw_srv).

-behaviour(gen_wp).

-export([
	start_link/0,
	send/3
	]).

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


-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("k_mailbox/include/pending_item.hrl").
-include_lib("gen_wp/include/gen_wp_spec.hrl").

-record(state, {
	chan :: pid(),
	reply_to :: binary()
	}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_wp:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(binary(), #k_mb_pending_item{}, integer()) -> ok.
send(QName, Item, TimeOut) ->
	gen_wp:call(?MODULE, {send, Item, QName, TimeOut}, infinity).

%% GenWP Callback Functions Definitions

init([]) ->
	Chan = k_mb_amqp_pool:open_channel(),
	link(Chan),
	{ok, ReplyTo} = application:get_env(reply_to),
	{ok, #state{chan = Chan, reply_to = ReplyTo}}.

handle_call(Msg = {send, _Item, _QName, _TimeOut}, _From, State = #state{}) ->
	% ?log_debug("Msg: ~p", [Msg]),
	{fork, {Msg, State}, State#state{}};
handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_fork_call(_Arg, {{send, Item, QName, TimeOut}, S = #state{}}, _ReplyTo, _WP) ->
	#state{
		chan = Chan,
		reply_to = ReplyTo
		} = S,

	#k_mb_pending_item{
		item_id = ItemID,
		content_type = CT,
		content_body = Payload
		} = Item,

	MesID = list_to_binary(ItemID),
	BasicProps =
	prepare_basic_props([{message_id, MesID}, {correlation_id, MesID}, {reply_to, ReplyTo}, {content_type, CT}]),

	ok = k_mb_amqp_funs:basic_publish(Chan, QName, Payload, BasicProps),
	% ?log_debug("basic_publish ok", []),
	Response = k_mb_amqp_consumer_srv:get_response(ItemID, TimeOut),
	{reply, Response, normal};
handle_fork_call(_Arg, _Msg, _ReplyTo, _WP) ->
	{error, unexpected_call}.

handle_fork_cast(_Arg, _Msg, _WP) ->
	{error, unexpected_cast}.

handle_child_forked(_Task, _Child, ModState) ->
	{noreply, ModState}.

handle_child_terminated(_Reason, _Task, _Child, ModState) ->
	{noreply, ModState}.

%% Internal functions

prepare_basic_props(Props) ->
	#'P_basic'{
		message_id = proplists:get_value(message_id, Props),
		correlation_id = proplists:get_value(correlation_id, Props),
		content_type = proplists:get_value(content_type, Props),
		content_encoding = proplists:get_value(content_encoding, Props),
		% delivery_mode = proplists:get_value(delivery_mode, Props, 2),
		reply_to = proplists:get_value(reply_to, Props),
		expiration = proplists:get_value(expiration, Props),
		timestamp = proplists:get_value(timestamp, Props),
		app_id = <<"kelly">>
		% headers,priority,type,user_id,cluster_id
		}.
