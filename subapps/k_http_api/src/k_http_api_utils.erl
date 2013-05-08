-module(k_http_api_utils).

-export([
	convert_datetime/1
]).

-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

%% convert_datetime(<<"2012-12-11T13:20">>) => {{2012,12,11},{13,20,0}}.
%% convert_datetime(<<"2012-12-11T13:20:15">>) => {{2012,12,11},{13,20,15}}.
-spec convert_datetime(binary()) -> calendar:datetime().
convert_datetime(DateTimeBin) ->
	DateTime = binary_to_list(DateTimeBin),
	DateTimeList = string:tokens(DateTime, [$-, $T, $:]),
	Result = [list_to_integer(List) || List <- DateTimeList],
	case Result of
		[Year, Month, Day, Hours, Minutes] ->
			{{Year, Month, Day}, {Hours, Minutes, 0}};
		[Year, Month, Day, Hours, Minutes, Seconds] ->
			{{Year, Month, Day}, {Hours, Minutes, Seconds}}
	end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

convert_datetime_test_() ->
	[
		?_assertEqual({{2012,12,11},{13,20,00}}, convert_datetime(<<"2012-12-11T13:20">>)),
		?_assertEqual({{2012,12,11},{13,20,15}}, convert_datetime(<<"2012-12-11T13:20:15">>))
	].

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
