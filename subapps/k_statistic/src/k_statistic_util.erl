-module(k_statistic_util).

-export([
	msg_stats_slice_path/1,
	msg_stats_slice_path/2,

	status_stats_slice_path/1,

	incoming_msg_stats_slice_path/1,

	write_term_to_file/2,
	read_term_from_file/1
]).

-include("application.hrl").

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
