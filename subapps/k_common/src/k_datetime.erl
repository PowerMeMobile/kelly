-module(k_datetime).

-export([
	utc_unix_epoch/0,
	utc_timestamp/0,

	timestamp_to_unix_epoch/1,
	unix_epoch_to_timestamp/1,

	datetime_to_unix_epoch/1,
	unix_epoch_to_datetime/1,

	timestamp_to_milliseconds/1,
	milliseconds_to_timestamp/1,

	datetime_to_iso_8601/1
]).

-include("application.hrl").

-type unix_epoch() :: integer().

%%%%%
%% Datetime
%%%%%

-spec utc_unix_epoch() -> unix_epoch().
utc_unix_epoch() ->
	timestamp_to_unix_epoch(os:timestamp()).

-spec utc_timestamp() -> erlang:timestamp().
utc_timestamp() ->
	os:timestamp().

-spec timestamp_to_unix_epoch(erlang:timestamp()) -> unix_epoch().
timestamp_to_unix_epoch({M, S, _}) ->
	M * 1000000 + S.

-spec unix_epoch_to_timestamp(unix_epoch()) -> os:timestamp().
unix_epoch_to_timestamp(UnixEpoch) ->
	M = UnixEpoch div 1000000,
	S = UnixEpoch rem 1000000,
	{M, S, 0}.

-spec datetime_to_unix_epoch(calendar:datetime()) -> unix_epoch().
datetime_to_unix_epoch({Date, Time}) ->
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({Date, Time}) -
	calendar:datetime_to_gregorian_seconds(ReferenceDate).

-spec unix_epoch_to_datetime(unix_epoch()) -> calendar:datetime().
unix_epoch_to_datetime(UnixTime) ->
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

-spec datetime_to_iso_8601(calendar:datetime()) -> string().
datetime_to_iso_8601({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
		io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
			[Year, Month, Day, Hour, Min, Sec])).
