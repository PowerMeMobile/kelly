-module(k_mb_amqp_producer_srv).

-behaviour(gen_wp).

-export([
	start_link/0,
	send/4
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
-include_lib("gen_wp/include/gen_wp_spec.hrl").
-include("pending_item.hrl").

-record(state, {
	chan :: pid(),
	reply_to :: binary()
	}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_wp:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(binary(), binary(), bitstring(), bitstring()) -> ok.
send(ID, Payload, QName, ContentType) ->
	gen_wp:call(?MODULE, {send, ID, Payload, QName, ContentType}, infinity).

%% GenWP Callback Functions Definitions

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ReplyTo = k_mb_config:get_env(reply_to),
	{ok, #state{chan = Chan, reply_to = ReplyTo}}.

handle_call(Msg = {send, _ID, _Payload, _QName, _ContentType}, _From, State = #state{}) ->
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

handle_fork_call(_Arg, {{send, ID, Payload, QName, ContentType}, S = #state{}}, _ReplyTo, _WP) ->
	#state{
		chan = Chan,
		reply_to = ReplyTo
		} = S,
	RMQMessageID = list_to_binary(uuid:to_string(ID)),
	BasicPropsPropListn =
		[{message_id, RMQMessageID},
		{correlation_id, RMQMessageID},
		{reply_to, ReplyTo},
		{content_type, ContentType}],
	ok = rmql:basic_publish(Chan, QName, Payload, BasicPropsPropListn),
	Response = k_mb_amqp_consumer_srv:get_response(ID),
	{reply, Response, normal};

handle_fork_call(_Arg, _Msg, _ReplyTo, _WP) ->
	{error, unexpected_call}.

handle_fork_cast(_Arg, _Msg, _WP) ->
	{error, unexpected_cast}.

handle_child_forked(_Task, _Child, ModState) ->
	{noreply, ModState}.

handle_child_terminated(_Reason, _Task, _Child, ModState) ->
	{noreply, ModState}.

%% ===================================================================
%% Internal
%% ===================================================================

%% build_funnel_incoming_sms_dto(ID, SenderAddr = #addr{}, DestAddr, Message, Encoding) ->
%% 	SenderAddrDTO = #addr_dto{
%% 		addr = SenderAddr#addr.addr,
%% 		ton = SenderAddr#addr.ton,
%% 		npi = SenderAddr#addr.npi
%% 	},
%% 	build_funnel_incoming_sms_dto(ID, SenderAddrDTO, DestAddr, Message, Encoding);
%% build_funnel_incoming_sms_dto(ID, SenderAddr, DestAddr = #addr{}, Message, Encoding) ->
%% 	DestAddrDTO = #addr_dto{
%% 		addr = DestAddr#addr.addr,
%% 		ton = DestAddr#addr.ton,
%% 		npi = DestAddr#addr.npi
%% 	},
%% 	build_funnel_incoming_sms_dto(ID, SenderAddr, DestAddrDTO, Message, Encoding);
%% build_funnel_incoming_sms_dto(BatchId, SenderAddr, DestAddr, Message, Encoding) ->
%% 	Msg = #funnel_incoming_sms_message_dto{
%% 		source = SenderAddr,
%% 		dest = DestAddr,
%% 		data_coding = Encoding,
%% 		message = Message
%% 	},
%% 	Batch = #funnel_incoming_sms_dto{
%% 		id = BatchId,
%% 		messages = [Msg]
%% 	},
%% 	{ok, Binary} = adto:encode(Batch),
%% 	Binary.
