-module(k_datetime).

-export([
	utc_time/0,
	utc_timestamp/0,
	utc_unixepoch/0,

	timestamp_to_datetime/1,
	datetime_to_timestamp/1,

	timestamp_to_unixepoch/1,
	unixepoch_to_timestamp/1,

	datetime_to_unixepoch/1,
	unixepoch_to_datetime/1,

	timestamp_to_milliseconds/1,
	milliseconds_to_timestamp/1,

	datetime_to_iso8601/1
]).

-include("application.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) == 62167219200
-define(GREGORIAN_SECS_BEFORE_UNIX_EPOCH, 62167219200).

-type datetime() :: calendar:datetime().
-type timestamp() :: erlang:timestamp().
-type unixepoch() :: pos_integer().

%% ===================================================================
%% API
%% ===================================================================

-spec utc_time() -> datetime().
utc_time() ->
	k_time_server:get_utc_time().

-spec utc_timestamp() -> timestamp().
utc_timestamp() ->
	k_time_server:get_utc_timestamp().

-spec utc_unixepoch() -> unixepoch().
utc_unixepoch() ->
	timestamp_to_unixepoch(utc_timestamp()).

-spec timestamp_to_datetime(timestamp()) -> datetime().
timestamp_to_datetime({M, S, _}) ->
	Secs = M * 1000000 + S + ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	calendar:gregorian_seconds_to_datetime(Secs).

-spec datetime_to_timestamp(datetime()) -> timestamp().
datetime_to_timestamp(Datetime) ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	unixepoch_to_timestamp(UnixEpoch).

-spec timestamp_to_unixepoch(timestamp()) -> unixepoch().
timestamp_to_unixepoch({M, S, _}) ->
	M * 1000000 + S.

-spec unixepoch_to_timestamp(unixepoch()) -> timestamp().
unixepoch_to_timestamp(UnixEpoch) ->
	M = UnixEpoch div 1000000,
	S = UnixEpoch rem 1000000,
	{M, S, 0}.

-spec datetime_to_unixepoch(datetime()) -> unixepoch().
datetime_to_unixepoch({Date, Time}) ->
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({Date, Time}) -
	calendar:datetime_to_gregorian_seconds(ReferenceDate).

-spec unixepoch_to_datetime(unixepoch()) -> datetime().
unixepoch_to_datetime(UnixTime) ->
    ReferenceDate = {{1970,1,1}, {0,0,0}},
	TotalSeconds = calendar:datetime_to_gregorian_seconds(ReferenceDate) + UnixTime,
	calendar:gregorian_seconds_to_datetime(TotalSeconds).

-spec timestamp_to_milliseconds(timestamp()) -> pos_integer().
timestamp_to_milliseconds({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

-spec milliseconds_to_timestamp(pos_integer()) -> timestamp().
milliseconds_to_timestamp(MS) ->
    Secs = MS div 1000,
    {Secs div 1000000, Secs rem 1000000, (MS rem 1000) * 1000}.

-spec datetime_to_iso8601(datetime()) -> string().
datetime_to_iso8601({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
		io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
			[Year, Month, Day, Hour, Min, Sec])).
