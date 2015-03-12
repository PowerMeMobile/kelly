-module(k_defers_publisher).

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0,
    publish/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include("application.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(unconfirmed, {
    id   :: integer(),
    from :: term()
}).

-record(st, {
    chan         :: pid(),
    chan_mon_ref :: reference(),
    next_id = 1  :: integer()
}).

-record('DOWN',{
    ref            :: reference(),
    type = process :: process,
    object         :: pid(),
    info           :: term() | noproc | noconnection
}).

-type payloadz() :: binary().
-type req_id() :: uuid().
-type gateway_id() :: uuid().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec publish(req_id(), gateway_id(), payloadz()) -> ok.
publish(ReqId, GtwId, PayloadZ) ->
    gen_server:call(?MODULE, {ReqId, GtwId, PayloadZ}, 60000).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, ordered_set, {keypos, 2}]),
    case setup_chan(#st{}) of
        {ok, St} ->
            {ok, St};
        unavailable ->
            ?log_error("Channel initialization failed with: unavailable", []),
            {stop, amqp_unavailable}
    end.

handle_call({ReqId, GtwId, PayloadZ}, From, St = #st{}) ->
    {Headers, RoutingKey} = headers_and_routing_key(GtwId),
    Props = [
        {content_type, <<"SmsReqV1z">>},
        {delivery_mode, 2},
        {priority, 1},
        {message_id, ReqId},
        {headers, Headers}
    ],
    Channel = St#st.chan,
    ok = rmql:basic_publish(Channel, RoutingKey, PayloadZ, Props),
    true = ets:insert(?MODULE, #unconfirmed{id = St#st.next_id, from = From}),
    {noreply, St#st{next_id = St#st.next_id + 1}};

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(Req, St) ->
    {stop, {unexpected_cast, Req}, St}.

handle_info(#'DOWN'{ref = Ref, info = Info}, St = #st{chan_mon_ref = Ref}) ->
    ?log_error("Channel down: ~p", [Info]),
    {stop, amqp_channel_down, St};

handle_info(Confirm, St) when is_record(Confirm, 'basic.ack');
                              is_record(Confirm, 'basic.nack') ->
    handle_confirm(Confirm),
    {noreply, St};

handle_info(_Info, St) ->
    {stop, unexpected_info, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Public Confirms
%% ===================================================================

headers_and_routing_key(GtwId) ->
    {ok, SmsReqQueue} = application:get_env(k_handlers, kelly_sms_request_queue),
    {ok, GtwQueueFmt} = application:get_env(k_handlers, just_gateway_queue_fmt),
    GtwQueue = binary:replace(GtwQueueFmt, <<"%id%">>, GtwId),
    %% use rabbitMQ 'CC' extention to avoid
    %% doubling publish confirm per 1 request
    CC = {<<"CC">>, array, [{longstr, GtwQueue}]},
    {[CC], SmsReqQueue}.

handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = false}) ->
    reply_to(DTag, ok);
handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = true}) ->
    reply_up_to(DTag, ok);
handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = false}) ->
    reply_to(DTag, {error, nack});
handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = true}) ->
    reply_up_to(DTag, {error, nack}).

reply_up_to(DTag, Reply) ->
    Ids = unconfirmed_ids_up_to(DTag),
    [reply_to(Id, Reply) || Id <- Ids].

reply_to(DTag, Reply) when is_integer(DTag) ->
    [Unconf] = ets:lookup(?MODULE, DTag),
    gen_server:reply(Unconf#unconfirmed.from, Reply),
    true = ets:delete(?MODULE, Unconf#unconfirmed.id).

unconfirmed_ids_up_to(UpToId) ->
    case ets:first(?MODULE) of
        '$end_of_table' ->
            [];
        FirstId ->
            unconfirmed_ids_up_to(UpToId, [], FirstId)
    end.

unconfirmed_ids_up_to(UpToId, Acc, LastId) when LastId =< UpToId ->
    case ets:next(?MODULE, LastId) of
        '$end_of_table' ->
            [LastId | Acc];
        NextId ->
            unconfirmed_ids_up_to(UpToId, [LastId | Acc], NextId)
    end;
unconfirmed_ids_up_to(_Uuid, Acc, _LastId) ->
    Acc.

%% ===================================================================
%% Internal
%% ===================================================================

setup_chan(St = #st{}) ->
    {ok, SmsReqQueue} = application:get_env(k_handlers, kelly_sms_request_queue),
    case rmql:channel_open() of
        {ok, Channel} ->
            ChanMonRef = erlang:monitor(process, Channel),
            amqp_channel:register_confirm_handler(Channel, self()),
            #'confirm.select_ok'{} = amqp_channel:call(Channel, #'confirm.select'{}),
            ok = rmql:queue_declare(Channel, SmsReqQueue, []),
            {ok, St#st{chan = Channel, chan_mon_ref = ChanMonRef}};
        unavailable ->
            unavailable
    end.
