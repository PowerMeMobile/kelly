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
	read_terms_from_files/1,
	read_terms_from_files_with/2,

	%% statistic utils
	get_timestamp_list/2,
	get_timestamp_list/3,
	get_timestamp_ranges/3,
	align_time_range/2,
	align_time_range/3,
	get_file_list_with/3,
	timestamp_to_iso_8601/1
]).

-include("application.hrl").

-type unix_epoch() :: pos_integer().

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

-spec msg_stats_slice_path(unix_epoch()) -> file:filename().
msg_stats_slice_path(Timestamp) ->
	slice_path("msg-stats/~p.dat", [Timestamp]).

report_type_to_index(customers) -> 1;
report_type_to_index(networks) -> 2.

-spec msg_stats_slice_path(unix_epoch(), atom()) -> file:filename().
msg_stats_slice_path(Timestamp, ReportType) ->
	slice_path("msg-stats/~p-~p.dat", [Timestamp, report_type_to_index(ReportType)]).

-spec status_stats_slice_path(unix_epoch()) -> file:filename().
status_stats_slice_path(Timestamp) ->
	slice_path("status-stats/~p.dat", [Timestamp]).

-spec incoming_msg_stats_slice_path(unix_epoch()) -> file:filename().
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

-spec read_terms_from_files(Filenames::[file:filename()]) -> [any()].
read_terms_from_files(Filenames) ->
	read_terms_from_files_with(Filenames, fun(Id) -> Id end).

-spec read_terms_from_files_with(
	Filenames::[file:filename()],
	MapTermFun::fun((A::term()) -> B::term())
) -> [any()].
read_terms_from_files_with(Filenames, MapTermFun) ->
	lists:foldr(
		fun(Filename, Acc) ->
			case k_statistic_utils:read_term_from_file(Filename) of
				{ok, []} ->
					Acc;
				{ok, Records} ->
					lists:map(MapTermFun, Records) ++ Acc;
				{error, _Reason} ->
					%?log_debug("Missing file: ~p", [Filename])
					Acc
			end
		end,
		[],
		Filenames).

%% ===================================================================
%% Statistic utils
%% ===================================================================

-spec get_timestamp_list(From::unix_epoch(), To::unix_epoch()) -> [unix_epoch()].
get_timestamp_list(From, To) when From < To ->
	Step = stats_report_frequency(),
	get_timestamp_list(From, To, Step).

-spec get_timestamp_list(From::unix_epoch(), To::unix_epoch(), Step::pos_integer()) -> [unix_epoch()].
get_timestamp_list(From, To, Step) when From < To ->
	{FromFloor, ToCeiling} = align_time_range(From, To),
	List = lists:seq(FromFloor, ToCeiling, Step),
	case lists:last(List) < To of
		true -> List ++ [To];
		false -> List
	end.

-spec get_timestamp_ranges(From::unix_epoch(), To::unix_epoch(), Step::pos_integer()) -> [{unix_epoch(), unix_epoch()}].
get_timestamp_ranges(From, To, Step) when From < To ->
	Timestamps = get_timestamp_list(From, To, Step),
	k_lists:make_ranges(Timestamps).

-spec align_time_range(From::unix_epoch(), To::unix_epoch()) ->
	{FromFloor::unix_epoch(), ToCeiling::unix_epoch()}.
align_time_range(From, To) ->
	Step = stats_report_frequency(),
	align_time_range(From, To, Step).

-spec align_time_range(From::unix_epoch(), To::unix_epoch(), Step::pos_integer()) ->
	{FromFloor::unix_epoch(), ToCeiling::unix_epoch()}.
align_time_range(From, To, Step) ->
	FromFloor = From - From rem Step,
	ToCeiling = case To rem Step of
					0 -> To;
					Rem -> To - Rem + Step
				end,
	{FromFloor, ToCeiling}.

-spec get_file_list_with(
	From::unix_epoch(),
	To::unix_epoch(),
	Fun::fun((Timestamp::unix_epoch()) -> file:filename())
) -> [file:filename()].
get_file_list_with(From, To, Fun) when From < To ->
	Timestamps = get_timestamp_list(From, To),
	lists:map(Fun, Timestamps).

-spec timestamp_to_iso_8601(Timestamp::unix_epoch()) -> string().
timestamp_to_iso_8601(Timestamp) ->
	k_datetime:datetime_to_iso_8601(
		k_datetime:unix_epoch_to_datetime(Timestamp)).
