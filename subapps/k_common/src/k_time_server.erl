-module(k_time_server).

%% API
-export([
	start_link/0,
	get_utc_datetime/0,
	get_utc_timestamp/0,

	set_utc_datetime/1
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
-include("gen_server_spec.hrl").
-include("logging.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) == 62167219200
-define(GREGORIAN_SECS_BEFORE_UNIX_EPOCH, 62167219200).

-record(state, {
	offset_secs :: non_neg_integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_utc_datetime() -> calendar:datetime1970().
get_utc_datetime() ->
	{ok, Datetime} = gen_server:call(?MODULE, get_utc_datetime),
	Datetime.

-spec get_utc_timestamp() -> erlang:timestamp().
get_utc_timestamp() ->
	{ok, Ts} = gen_server:call(?MODULE, get_utc_timestamp),
	Ts.

-spec set_utc_datetime(calendar:datetime1970() | 0) -> ok.
set_utc_datetime(Datetime) ->
	gen_server:cast(?MODULE, {set_utc_datetime, Datetime}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	OffsetSecs =
		case application:get_env(?APP, time_server) of
			{ok, Props} ->
				case proplists:get_value(set_utc_datetime, Props) of
					undefined -> 0;
					Datetime -> calc_offset(Datetime)
				end;
			_ -> 0
		end,
	{ok, #state{
		offset_secs = OffsetSecs
	}}.

handle_call(get_utc_datetime, _From, State = #state{
	offset_secs = 0
}) ->
	Datetime = calendar:universal_time(),
	{reply, {ok, Datetime}, State};

handle_call(get_utc_datetime, _From, State = #state{
	offset_secs = OffsetSecs
}) ->
	{NowSecs, _} = now_secs(),
	Secs = NowSecs - OffsetSecs + ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	Datetime = calendar:gregorian_seconds_to_datetime(Secs),
	{reply, {ok, Datetime}, State};

handle_call(get_utc_timestamp, _From, State = #state{
	offset_secs = 0
}) ->
	Ts = os:timestamp(),
	{reply, {ok, Ts}, State};

handle_call(get_utc_timestamp, _From, State = #state{
	offset_secs = OffsetSecs
}) ->
	{NowSecs, NowMc} = now_secs(),
	Secs = NowSecs - OffsetSecs,
	M = Secs div 1000000,
	S = Secs rem 1000000,
	Ts = {M, S, NowMc},
	{reply, {ok, Ts}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({set_utc_datetime, 0}, State = #state{}) ->
	{noreply, State#state{
		offset_secs = 0
	}};

handle_cast({set_utc_datetime, Datetime}, State = #state{}) ->
	OffsetSecs = calc_offset(Datetime),
	{noreply, State#state{
		offset_secs = OffsetSecs
	}};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

now_secs() ->
	{Mg, S, Mc} = os:timestamp(),
	{Mg * 1000000 + S, Mc}.

calc_offset(Datetime) ->
	{NowSecs, _} = now_secs(),
	Secs = calendar:datetime_to_gregorian_seconds(Datetime) - ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	NowSecs - Secs.
