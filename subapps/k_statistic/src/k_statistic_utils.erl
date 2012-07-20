-module(k_statistic_utils).

-export([
	stats_report_frequency/0,

	%% slice paths
	msg_stats_slice_path/1,
	msg_stats_slice_path/2,
	status_stats_slice_path/1,
	incoming_msg_stats_slice_path/1,

	%% read/write term
	write_term_to_file/2,
	read_term_from_file/1,

	%% statistic utils
	get_timestamp_list/2,
	get_timestamp_list/3,
	get_timestamp_ranges/3,
	align_time_range/2,
	align_time_range/3,
	get_file_list/3, % rename to get_file_list_with
	timestamp_to_iso_8601/1,

	%% common utils
	make_pair/2,
	remove/2,
	group/1,
	groupwith/2,
	make_ranges/1,
	make_frequencies/1
]).

-include("application.hrl").

%% ===================================================================
%% Slice frequency
%% ===================================================================

-spec stats_report_frequency() -> integer().
stats_report_frequency() ->
	60.

%% ===================================================================
%% Slice Paths
%% ===================================================================

-spec slice_path(string(), [any()]) -> file:filename().
slice_path(Fmt, Args) ->
	{ok, CWD} = file:get_cwd(),
	Path = lists:flatten(
		io_lib:format(
			"data/time-slices.d/" ++ Fmt, Args)),
	filename:join(CWD, Path).

-spec msg_stats_slice_path(os:timestamp()) -> file:filename().
msg_stats_slice_path(Timestamp) ->
	slice_path("msg-stats/~p.dat", [Timestamp]).

report_type_to_index(customers) -> 1;
report_type_to_index(networks) -> 2.

-spec msg_stats_slice_path(os:timestamp(), atom()) -> file:filename().
msg_stats_slice_path(Timestamp, ReportType) ->
	slice_path("msg-stats/~p-~p.dat", [Timestamp, report_type_to_index(ReportType)]).

-spec status_stats_slice_path(os:timestamp()) -> file:filename().
status_stats_slice_path(Timestamp) ->
	slice_path("status-stats/~p.dat", [Timestamp]).

-spec incoming_msg_stats_slice_path(os:timestamp()) -> file:filename().
incoming_msg_stats_slice_path(Timestamp) ->
	slice_path("incoming-msg-stats/~p.dat", [Timestamp]).

%% ===================================================================
%% Read/Write term
%% ===================================================================

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

%% ===================================================================
%% Statistic utils
%% ===================================================================

-spec get_timestamp_list(From::os:timestamp(), To::os:timestamp()) -> [os:timestamp()].
get_timestamp_list(From, To) when From < To ->
	Step = stats_report_frequency(),
	get_timestamp_list(From, To, Step).

-spec get_timestamp_list(From::os:timestamp(), To::os:timestamp(), Step::pos_integer()) -> [os:timestamp()].
get_timestamp_list(From, To, Step) when From < To ->
	{FromFloor, ToCeiling} = align_time_range(From, To),
	List = lists:seq(FromFloor, ToCeiling, Step),
	case lists:last(List) < To of
		true -> List ++ [To];
		false -> List
	end.

-spec get_timestamp_ranges(From::os:timestamp(), To::os:timestamp(), Step::pos_integer()) -> [{os:timestamp(), os:timestamp()}].
get_timestamp_ranges(From, To, Step) when From < To ->
	Timestamps = get_timestamp_list(From, To, Step),
	make_ranges(Timestamps).

-spec align_time_range(From::os:timestamp(), To::os:timestamp()) ->
	{FromFloor::os:timestamp(), ToCeiling::os:timestamp()}.
align_time_range(From, To) ->
	Step = stats_report_frequency(),
	align_time_range(From, To, Step).

-spec align_time_range(From::os:timestamp(), To::os:timestamp(), Step::pos_integer()) ->
	{FromFloor::os:timestamp(), ToCeiling::os:timestamp()}.
align_time_range(From, To, Step) ->
	FromFloor = From - From rem Step,
	ToCeiling = case To rem Step of
					0 -> To;
					Rem -> To - Rem + Step
				end,
	{FromFloor, ToCeiling}.

-spec get_file_list(
	From::os:timestamp(),
	To::os:timestamp(),
	Fun::fun((Timestamp::os:timestamp()) -> file:filename())
) -> [file:filename()].
get_file_list(From, To, Fun) when From < To ->
	Timestamps = get_timestamp_list(From, To),
	lists:map(Fun, Timestamps).

-spec timestamp_to_iso_8601(Timestamp::os:timestamp()) -> string().
timestamp_to_iso_8601(Timestamp) ->
	k_datetime:datetime_to_iso_8601(
		k_datetime:unix_epoch_to_datetime(Timestamp)).

%% make_pair(2, {a,b,c}) ==> {b,{a,c}}
%% make_pair(1, {a,b}) ==> {a,b}
-spec make_pair(KeyN::integer(), Tuple::tuple()) -> {Key::term(), Value::tuple()} | {Key::term(), Value::term()}.
make_pair(KeyN, Tuple) ->
	Key = element(KeyN, Tuple),
	ValueList = remove(KeyN, tuple_to_list(Tuple)),
	Value = case length(ValueList) of
				1 -> hd(ValueList);
				_ -> list_to_tuple(ValueList)
			end,
	{Key, Value}.

%% remove(2, "abcdef") ==> "acdef"
-spec remove(N::integer(), List::[term()]) -> [term()].
remove(_, []) -> [];
remove(1, [_|T]) -> T;
remove(N, [H|T]) -> [H | remove(N-1, T)].

%% group("Mississippi") ==> ["M","i","ss","i","ss","i","pp","i"]
-spec group([A]) -> [[A]].
group(List) ->
	groupwith(fun erlang:'=:='/2, List).

-spec groupwith(Eq::fun((A, A) -> boolean()), [A]) -> [[A]].
groupwith(_, []) ->
	[];
groupwith(Eq, [X|XS]) ->
	{YS, ZS} = lists:splitwith(fun(I) -> Eq(X, I) end, XS),
	[[X|YS] | groupwith(Eq, ZS)].

%% make_ranges([1,2,3,4,5]) ==> [{1,2},{2,3},{3,4},{4,5}]
-spec make_ranges([A]) -> [{A,A}].
make_ranges(List) ->
	make_ranges(List, []).
make_ranges([_|[]], Ranges) ->
	lists:reverse(Ranges);
make_ranges([F,S|T], Ranges) ->
	make_ranges([S|T], [{F,S}|Ranges]).

%% make_frequencies([1,2,3,2,3,3]) ==> [{1,1},{2,2},{3,3}]
-spec make_frequencies([A]) -> [{A, pos_integer()}].
make_frequencies(Timestamps) ->
	Groups = group(lists:sort(Timestamps)),
	lists:map(
		fun([H|_] = L) ->
			{H, length(L)}
		end,
		Groups).
