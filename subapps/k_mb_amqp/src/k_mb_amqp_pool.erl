-module(k_mb_amqp_pool).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include("otp_records.hrl").

-behaviour(gen_server).

%% API exports
-export([start_link/0,
         open_channel/0,
         close_channel/1]).

%% gen_server exports
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-record(st, {amqp_conn :: pid(), amqp_chans :: ets:tid()}).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open_channel() -> pid().
open_channel() ->
    gen_server:call(?MODULE, open_channel, infinity).

-spec close_channel(pid()) -> no_return().
close_channel(Chan) ->
    gen_server:cast(?MODULE, {close_channel, Chan}).

%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------

init([]) ->
    ?log_debug("amqp pool: initializing", []),
    case k_mb_amqp_funs:connection_start() of
        {ok, Conn} ->
            link(Conn),
            {ok, #st{amqp_conn = Conn, amqp_chans = ets:new(amqp_chans, [])}};
        {error, Reason} ->
            ?log_error("amqp pool: failed to start (~p)", [Reason]),
            {stop, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(open_channel, {Pid, _Tag}, St) ->
    case k_mb_amqp_funs:channel_open(St#st.amqp_conn) of
        {ok, Chan} ->
            Ref = monitor(process, Pid),
            ets:insert(St#st.amqp_chans, {Ref, Chan}),
            {reply, Chan, St};
        {error, Reason} ->
            {stop, Reason, St}
    end.

handle_cast({close_channel, Chan}, St) ->
    case ets:match(St#st.amqp_chans, {'$1', Chan}) of
        [[Ref]] ->
            demonitor(Ref),
            k_mb_amqp_funs:channel_close(Chan),
            ets:delete(St#st.amqp_chans, Ref);
        [] ->
            ignore
    end,
    {noreply, St}.

handle_info(#'DOWN'{ref = Ref}, St) ->
    case ets:lookup(St#st.amqp_chans, Ref) of
        [{_, Chan}] ->
            k_mb_amqp_funs:channel_close(Chan),
            ets:delete(St#st.amqp_chans, Ref);
        [] ->
            ignore
    end,
    {noreply, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
