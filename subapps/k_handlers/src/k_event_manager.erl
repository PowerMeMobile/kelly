-module(k_event_manager).

%% API
-export([
    start_link/0,
    notify_blacklist_changed/0,
    notify_customer_changed/2
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

-include("application.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("k_storage/include/customer.hrl").

-record(state, {
    connection,
    channel
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec notify_blacklist_changed() -> ok | {error, any()}.
notify_blacklist_changed() ->
    case gen_server:call(?MODULE, get_channel) of
        {ok, Channel} ->
            {ok, Exchange} = application:get_env(?APP, kelly_events_exchange),
            Payload = <<"BlacklistChanged">>,
            Props = [
                {content_type, <<"text/plain">>},
                {delivery_mode, 2}
            ],
            rmql:basic_publish(Channel, Exchange, Exchange, Payload, Props);
        {error, Reason} ->
            {error, Reason}
    end.

-spec notify_customer_changed(customer_uuid(), customer_id()) -> ok | {error, any()}.
notify_customer_changed(CustomerUuid, CustomerId) ->
    case gen_server:call(?MODULE, get_channel) of
        {ok, Channel} ->
            {ok, Exchange} = application:get_env(?APP, kelly_events_exchange),
            Payload = <<"CustomerChanged", ":", CustomerUuid/binary, ":", CustomerId/binary>>,
            Props = [
                {content_type, <<"text/plain">>},
                {delivery_mode, 2}
            ],
            rmql:basic_publish(Channel, Exchange, Exchange, Payload, Props);
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, Connection} = rmql:connection_start(),
    {ok, Channel} = rmql:channel_open(Connection),
    {ok, Exchange} = application:get_env(?APP, kelly_events_exchange),
    ok = rmql:exchange_declare(Channel, Exchange, fanout, true),
    {ok, #state{
        connection = Connection,
        channel = Channel
    }}.

handle_call(get_channel, _From, State = #state{channel = Channel}) ->
    {reply, {ok, Channel}, State};
handle_call(Request, _From, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
    {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
    {stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
    rmql:channel_close(Channel),
    rmql:connection_close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
