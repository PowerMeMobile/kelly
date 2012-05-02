-module(k_storage_util).

-export([
	msg_stats_file_path/1,
	gtw_stats_file_path/1,

	utc_unix_epoch/0,
	timestamp_to_unix_epoch/1,
	unix_epoch_to_timestamp/1,
	datetime_to_unix_epoch/1,
	unix_epoch_to_datetime/1,
	datetime_to_iso_8601/1,

	write_term_to_file/2,
	read_term_from_file/1
]).

-include("application.hrl").

-spec msg_stats_file_path(string()) -> string().
msg_stats_file_path(Filename) ->
	{ok, CWD} = file:get_cwd(),
	Path = lists:flatten(io_lib:format("data/time-slices.d/msg-stats/~s", [Filename])),
	filename:join(CWD, Path).

-spec gtw_stats_file_path(string()) -> string().
gtw_stats_file_path(Filename) ->
	{ok, CWD} = file:get_cwd(),
	Path = lists:flatten(io_lib:format("data/time-slices.d/gtw-stats/~s", [Filename])),
	filename:join(CWD, Path).

%%%%%
%% Datetime
%%%%%

-spec utc_unix_epoch() -> integer().
utc_unix_epoch() ->
	datetime_to_unix_epoch(calendar:universal_time()).

-spec timestamp_to_unix_epoch(erlang:timestamp()) -> integer().
timestamp_to_unix_epoch({M, S, _}) ->
	M * 1000000 + S.

-spec unix_epoch_to_timestamp(integer()) -> os:timestamp().
unix_epoch_to_timestamp(UnixEpoch) ->
	M = UnixEpoch div 1000000,
	S = UnixEpoch rem 1000000,
	{M, S, 0}.

-spec datetime_to_unix_epoch(calendar:datetime()) -> integer().
datetime_to_unix_epoch({Date, Time}) ->
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({Date, Time}) -
	calendar:datetime_to_gregorian_seconds(ReferenceDate).

-spec unix_epoch_to_datetime(integer()) -> calendar:datetime().
unix_epoch_to_datetime(UnixTime) ->
    ReferenceDate = {{1970,1,1}, {0,0,0}},
	TotalSeconds = calendar:datetime_to_gregorian_seconds(ReferenceDate) + UnixTime,
	calendar:gregorian_seconds_to_datetime(TotalSeconds).

-spec datetime_to_iso_8601(calendar:datetime()) -> string().
datetime_to_iso_8601({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
		io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
			[Year, Month, Day, Hour, Min, Sec])).

-type filename() :: string().
-spec write_term_to_file(Term::term(), Filename::filename()) -> ok | {error, any()}.
write_term_to_file(Term, Filename) ->
	Binary = term_to_binary(Term, [compressed]),
	file:write_file(Filename, Binary).

-spec read_term_from_file(Filename::filename()) -> {ok, term()} | {error, Reason::any()}.
read_term_from_file(Filename) ->
	case file:read_file(Filename) of
		{ok, Binary} ->
			{ok, binary_to_term(Binary)};
		Error ->
			Error
	end.
