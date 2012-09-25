-module(k_mb_amqp_consumer_srv).

-behaviour(gen_server).

-compile({no_auto_import, [now/0]}).

-include_lib("k_common/include/logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("k_common/include/FunnelAsn.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

%% pending workers
-record(pworker, {
	id,
	timestamp,
	from
}).

%% pending responses
-record(presponse, {
	id,
	timestamp
}).

-record(state, {
	chan :: pid(),
	queue :: binary(),
	tag :: binary(),
	pending_workers = [] :: [#pworker{}],
	pending_responses = [] :: [#presponse{}]
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	get_response/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_response(string()) -> {ok, delivered} | {error, timeout}.
get_response(MesID) ->
	Timeout = k_mb_config:get_env(request_timeout),
	try
		gen_server:call(?MODULE, {get_response, MesID}, Timeout)
	catch
		_:{timeout, _} -> {error, timeout};
		_:Error -> Error
	end.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	{ok, Chan} = rmql:channel_open(),
	{ok, QoS} = application:get_env(rmq_qos),
	ok = rmql:basic_qos(Chan, QoS),
	ReplyToQName = k_mb_config:get_env(reply_to),
	DeclareProps = [{durable, false}],
	ok = rmql:queue_declare(Chan, ReplyToQName, DeclareProps),
	NoAck = true,
	{ok, ConsumerTag} = rmql:basic_consume(Chan, ReplyToQName, NoAck),
	{ok, #state{tag = ConsumerTag, queue = ReplyToQName, chan = Chan}}.

handle_call({get_response, MesID}, From,
					State = #state{
								pending_workers = WList,
								pending_responses = RList}) ->
	Worker = #pworker{id = MesID, from = From, timestamp = now()},
	{ok, NRList, NWList} = process_worker_request(Worker, RList, WList),
	{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, State) ->
	{stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
	{stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{},
			 #amqp_msg{props = #'P_basic'{content_type = <<"BatchAck">>}, payload = Content}},
			 State = #state{
			 	pending_responses = RList,
				pending_workers = WList}) ->
	case 'FunnelAsn':decode('BatchAck', Content) of
		{ok, #'BatchAck'{batchId = ItemIDStr}} ->
			ItemID = uuid:to_binary(ItemIDStr),
			{ok, NRList, NWList} =
				process_response(ItemID, RList, WList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, AsnErr} ->
			?log_error("Failed to decode 'BatchAck' due to ~p : ~p", [AsnErr, Content]),
			{noreply, State}
	end;

handle_info(_Info, State) ->
	{stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------------------------------------------------------------------
% Internal Function Definitions
% ------------------------------------------------------------------

process_response(ItemID, RList, WList) ->
		case lists:keytake(ItemID, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, {ok, delivered}),
			{ok, purge(RList), purge(RestWorkerList)};

		false ->
			Response = #presponse{id = ItemID, timestamp = now()},
			{ok, [Response] ++ purge(RList), purge(WList)}
	end.

process_worker_request(Worker = #pworker{id = ItemID, from = From}, RList, WList) ->
	case lists:keytake(ItemID, #presponse.id, RList) of
		{value, #presponse{}, RestRespList} ->
			gen_server:reply(From, ok),
			{ok, purge(RestRespList), purge(WList)};
		false ->
			{ok, purge(RList), [Worker] ++ purge(WList)}
	end.

purge(List) ->
	ExpirationInterval = k_mb_config:get_env(request_timeout),
	purge(List, [], now() - ExpirationInterval/1000).

purge([], Acc, _Now) -> Acc;
purge([#pworker{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([#presponse{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([Item | RestList], Acc, Now) ->
	purge(RestList, [Item | Acc], Now).

now() ->
	 calendar:datetime_to_gregorian_seconds(calendar:local_time()).
