-module(k_datetime).

-export([
	utc_time/0,
	utc_timestamp/0,
	utc_unixepoch/0,

	timestamp_to_unixepoch/1,
	unixepoch_to_timestamp/1,

	datetime_to_unixepoch/1,
	unixepoch_to_datetime/1,

	timestamp_to_milliseconds/1,
	milliseconds_to_timestamp/1,

	datetime_to_iso8601/1
]).

-include("application.hrl").

-type unixepoch() :: pos_integer().

%%%%%
%% Datetime
%%%%%

-spec utc_time() -> calendar:datetime().
utc_time() ->
	k_time_server:get_utc_time().

-spec utc_timestamp() -> erlang:timestamp().
utc_timestamp() ->
	k_time_server:get_utc_timestamp().

-spec utc_unixepoch() -> unixepoch().
utc_unixepoch() ->
	timestamp_to_unixepoch(utc_timestamp()).

-spec timestamp_to_unixepoch(erlang:timestamp()) -> unixepoch().
timestamp_to_unixepoch({M, S, _}) ->
	M * 1000000 + S.

-spec unixepoch_to_timestamp(unixepoch()) -> erlang:timestamp().
unixepoch_to_timestamp(UnixEpoch) ->
	M = UnixEpoch div 1000000,
	S = UnixEpoch rem 1000000,
	{M, S, 0}.

-spec datetime_to_unixepoch(calendar:datetime()) -> unixepoch().
datetime_to_unixepoch({Date, Time}) ->
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({Date, Time}) -
	calendar:datetime_to_gregorian_seconds(ReferenceDate).

-spec unixepoch_to_datetime(unixepoch()) -> calendar:datetime().
unixepoch_to_datetime(UnixTime) ->
    ReferenceDate = {{1970,1,1}, {0,0,0}},
	TotalSeconds = calendar:datetime_to_gregorian_seconds(ReferenceDate) + UnixTime,
	calendar:gregorian_seconds_to_datetime(TotalSeconds).

-spec timestamp_to_milliseconds(erlang:timestamp()) -> pos_integer().
timestamp_to_milliseconds({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

-spec milliseconds_to_timestamp(pos_integer()) -> erlang:timestamp().
milliseconds_to_timestamp(MS) ->
    Secs = MS div 1000,
    {Secs div 1000000, Secs rem 1000000, (MS rem 1000) * 1000}.

-spec datetime_to_iso8601(calendar:datetime()) -> string().
datetime_to_iso8601({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
		io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
			[Year, Month, Day, Hour, Min, Sec])).
