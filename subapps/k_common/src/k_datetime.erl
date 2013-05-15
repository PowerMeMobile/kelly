-module(k_datetime).

-export([
	utc_datetime/0,
	utc_timestamp/0,
	utc_unixepoch/0,
	utc_string/0,

	timestamp_to_datetime/1,
	timestamp_to_unixepoch/1,
	timestamp_to_utc_string/1,

	datetime_to_timestamp/1,
	datetime_to_unixepoch/1,
	datetime_to_utc_string/1,
	datetime_to_iso8601/1,

	unixepoch_to_timestamp/1,
	unixepoch_to_datetime/1,
	unixepoch_to_iso8601/1,

	utc_string_to_datetime/1,
	utc_string_to_timestamp/1,

	iso8601_to_datetime/1
]).

-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) == 62167219200
-define(GREGORIAN_SECS_BEFORE_UNIX_EPOCH, 62167219200).

-type datetime() :: calendar:datetime().
-type timestamp() :: erlang:timestamp().
-type unixepoch() :: pos_integer().

%% ===================================================================
%% API Current UTC
%% ===================================================================

-spec utc_datetime() -> datetime().
utc_datetime() ->
	k_time_server:get_utc_datetime().

-spec utc_timestamp() -> timestamp().
utc_timestamp() ->
	k_time_server:get_utc_timestamp().

-spec utc_unixepoch() -> unixepoch().
utc_unixepoch() ->
	timestamp_to_unixepoch(utc_timestamp()).

-spec utc_string() -> binary().
utc_string() ->
	timestamp_to_utc_string(utc_timestamp()).

%% ===================================================================
%% API Conversions
%% ===================================================================

-spec timestamp_to_datetime(timestamp()) -> datetime().
timestamp_to_datetime({M, S, _}) ->
	Secs = M * 1000000 + S + ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	calendar:gregorian_seconds_to_datetime(Secs).

-spec timestamp_to_unixepoch(timestamp()) -> unixepoch().
timestamp_to_unixepoch({M, S, _}) ->
	M * 1000000 + S.

-spec timestamp_to_utc_string(timestamp()) -> binary().
timestamp_to_utc_string(Timestamp) ->
	datetime_to_utc_string(timestamp_to_datetime(Timestamp)).

-spec datetime_to_timestamp(datetime()) -> timestamp().
datetime_to_timestamp(Datetime) ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds(Datetime) -
		?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
	unixepoch_to_timestamp(UnixEpoch).

-spec datetime_to_unixepoch(datetime()) -> unixepoch().
datetime_to_unixepoch(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) -
	?GREGORIAN_SECS_BEFORE_UNIX_EPOCH.

-spec datetime_to_utc_string(datetime()) -> binary().
datetime_to_utc_string({{Y,Mon,D},{H,Min,S}}) ->
	list_to_binary(
		lists:concat([pad(Y rem 100), pad(Mon), pad(D), pad(H), pad(Min), pad(S)])
	).

-spec datetime_to_iso8601(datetime()) -> binary().
datetime_to_iso8601({{Y,Mon,D},{H,Min,S}}) ->
    list_to_binary(lists:flatten(
		io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
			[Y, Mon, D, H, Min, S]))).

-spec unixepoch_to_timestamp(unixepoch()) -> timestamp().
unixepoch_to_timestamp(UnixEpoch) ->
	M = UnixEpoch div 1000000,
	S = UnixEpoch rem 1000000,
	{M, S, 0}.

-spec unixepoch_to_iso8601(unixepoch()) -> binary().
unixepoch_to_iso8601(UnixEpoch) ->
	datetime_to_iso8601(unixepoch_to_datetime(UnixEpoch)).

-spec unixepoch_to_datetime(unixepoch()) -> datetime().
unixepoch_to_datetime(UnixEpoch) ->
	TotalSeconds = ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH + UnixEpoch,
	calendar:gregorian_seconds_to_datetime(TotalSeconds).

-spec utc_string_to_datetime(binary() | string()) -> datetime().
utc_string_to_datetime(Bin) when is_binary(Bin) ->
	utc_string_to_datetime(binary_to_list(Bin));
utc_string_to_datetime(List) when is_list(List) ->
	[Y, Mon, D, H, Min, S] = split_utc_string(List),
	Year = if
		Y >= 38 andalso Y =< 99 ->
			Y + 1900;
		Y >= 00 andalso Y =< 37 ->
			Y + 2000
	end,
	{{Year,Mon,D},{H,Min,S}}.

-spec utc_string_to_timestamp(binary() | string()) -> timestamp().
utc_string_to_timestamp(UTCString) ->
	datetime_to_timestamp(utc_string_to_datetime(UTCString)).

-spec iso8601_to_datetime(binary()) -> datetime().
iso8601_to_datetime(Bin) ->
	List = binary_to_list(Bin),
	Tokens = string:tokens(List, [$-, $T, $:]),
	Result = [list_to_integer(Token) || Token <- Tokens],
	case Result of
		[Year, Month, Day, Hours, Minutes] ->
			{{Year, Month, Day}, {Hours, Minutes, 0}};
		[Year, Month, Day, Hours, Minutes, Seconds] ->
			{{Year, Month, Day}, {Hours, Minutes, Seconds}}
	end.

%% ===================================================================
%% Internal
%% ===================================================================

split_utc_string(List) ->
	split_utc_string(List, []).

split_utc_string([], Acc) ->
	lists:reverse(Acc);
split_utc_string(List, Acc) ->
	{Pre, Post} = lists:split(2, List),
	split_utc_string(Post, [list_to_integer(Pre) | Acc]).

pad(N) when N > 9 ->
    N;
pad(N) ->
    [$0, N + 48].

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

timestamp_to_datetime_test_() ->
	TS0 = {0,0,0},
	DT0 = {{1970,1,1},{0,0,0}},
	TS = {1368,23819,573663},
	DT = {{2013,5,8},{14,36,59}},
	[
		?_assertEqual(DT0, timestamp_to_datetime(TS0)),
		?_assertEqual(DT, timestamp_to_datetime(TS))
	].

timestamp_to_unixepoch_test_() ->
	TS0 = {0,0,0},
	UE0 = 0,
	TS = {1368,23819,573663},
	UE = 1368023819,
	[
		?_assertEqual(UE0, timestamp_to_unixepoch(TS0)),
		?_assertEqual(UE, timestamp_to_unixepoch(TS))
	].

timestamp_to_utc_string_test() ->
	TS = {1368,23819,573663},
	UC = <<"130508143659">>,
	?assertEqual(UC, timestamp_to_utc_string(TS)).

datetime_to_timestamp_test_() ->
	TS0 = {0,0,0},
	DT0 = {{1970,1,1},{0,0,0}},
	TS = {1368,23819,0},
	DT = {{2013,5,8},{14,36,59}},
	[
		?_assertEqual(TS0, datetime_to_timestamp(DT0)),
		?_assertEqual(TS, datetime_to_timestamp(DT))
	].

datetime_to_unixepoch_test_() ->
	UE0 = 0,
	DT0 = {{1970,1,1},{0,0,0}},
	UE = 1368023819,
	DT = {{2013,5,8},{14,36,59}},
	[
		?_assertEqual(UE0, datetime_to_unixepoch(DT0)),
		?_assertEqual(UE, datetime_to_unixepoch(DT))
	].

datetime_to_utc_string_test_() ->
	DT00 = {{2000,01,02},{03,04,05}},
	UC00 = <<"000102030405">>,
	DT37 = {{2037,01,02},{03,04,05}},
	UC37 = <<"370102030405">>,
	DT38 = {{1938,01,02},{03,04,05}},
	UC38 = <<"380102030405">>,
	DT99 = {{1999,01,02},{03,04,05}},
	UC99 = <<"990102030405">>,
	[
		?_assertEqual(UC00, datetime_to_utc_string(DT00)),
		?_assertEqual(UC37, datetime_to_utc_string(DT37)),
		?_assertEqual(UC38, datetime_to_utc_string(DT38)),
		?_assertEqual(UC99, datetime_to_utc_string(DT99))
	].

datetime_to_iso8601_test() ->
	DT = {{2013,5,8},{14,36,59}},
	ISO = <<"2013-05-08T14:36:59">>,
	?assertEqual(ISO, datetime_to_iso8601(DT)).

unixepoch_to_timestamp_test_() ->
	TS0 = {0,0,0},
	UE0 = 0,
	TS = {1368,23819,0},
	UE = 1368023819,
	[
		?_assertEqual(TS0, unixepoch_to_timestamp(UE0)),
		?_assertEqual(TS, unixepoch_to_timestamp(UE))
	].

unixepoch_to_datetime_test_() ->
	UE0 = 0,
	DT0 = {{1970,1,1},{0,0,0}},
	UE = 1368023819,
	DT = {{2013,5,8},{14,36,59}},
	[
		?_assertEqual(DT0, unixepoch_to_datetime(UE0)),
		?_assertEqual(DT, unixepoch_to_datetime(UE))
	].

unixepoch_to_iso8601_test() ->
	UE = 1368023819,
	ISO = <<"2013-05-08T14:36:59">>,
	?assertEqual(ISO, unixepoch_to_iso8601(UE)).

utc_string_to_datetime_test_() ->
	DT00 = {{2000,01,02},{03,04,05}},
	UC00 = <<"000102030405">>,
	DT37 = {{2037,01,02},{03,04,05}},
	UC37 = <<"370102030405">>,
	DT38 = {{1938,01,02},{03,04,05}},
	UC38 = <<"380102030405">>,
	DT99 = {{1999,01,02},{03,04,05}},
	UC99 = <<"990102030405">>,
	[
		?_assertEqual(DT00, utc_string_to_datetime(UC00)),
		?_assertEqual(DT37, utc_string_to_datetime(UC37)),
		?_assertEqual(DT38, utc_string_to_datetime(UC38)),
		?_assertEqual(DT99, utc_string_to_datetime(UC99)),
		?_assertEqual(DT00, utc_string_to_datetime(binary_to_list(UC00)))
	].

utc_string_to_timestamp_test() ->
	TS = {1357,95845,0},
	UC = <<"130102030405">>,
	?assertEqual(TS, utc_string_to_timestamp(UC)).

iso8601_to_datetime_test_() ->
	ISO1 = <<"2012-12-11T13:20">>,
	DT1 = {{2012,12,11},{13,20,0}},
	ISO2 = <<"2012-12-11T13:20:15">>,
	DT2 = {{2012,12,11},{13,20,15}},
	[
		?_assertEqual(DT1, iso8601_to_datetime(ISO1)),
		?_assertEqual(DT2, iso8601_to_datetime(ISO2))
	].

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
