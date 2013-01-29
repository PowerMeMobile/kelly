-module(k_time_server).

%% API
-export([
	start_link/0,
	get_utc_time/0,
	set_utc_time/1
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
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("k_common/include/logging.hrl").

-record(state, {
	offset_secs :: non_neg_integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_utc_time() -> calendar:datetime().
get_utc_time() ->
	{ok, Datetime} = gen_server:call(?MODULE, get_utc_time),
	Datetime.

-spec set_utc_time(calendar:datetime()) -> ok.
set_utc_time(Datetime) ->
	gen_server:cast(?MODULE, {set_utc_time, Datetime}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	OffsetSecs =
		case application:get_env(?APP, time_server) of
			{ok, Props} ->
				case proplists:get_value(set_utc_time, Props) of
					undefined -> 0;
					Datetime -> calc_offset(Datetime)
				end;
			_ -> 0
		end,
	{ok, #state{
		offset_secs = OffsetSecs
	}}.

handle_call(get_utc_time, _From, State = #state{
	offset_secs = 0
}) ->
	Datetime = calendar:universal_time(),
	{reply, {ok, Datetime}, State};

handle_call(get_utc_time, _From, State = #state{
	offset_secs = OffsetSecs
}) ->
	NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	Datetime = calendar:gregorian_seconds_to_datetime(NowSecs - OffsetSecs),
	{reply, {ok, Datetime}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({set_utc_time, Datetime}, State = #state{}) ->
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

calc_offset(Datetime) ->
	NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	Secs = calendar:datetime_to_gregorian_seconds(Datetime),
	NowSecs - Secs.
