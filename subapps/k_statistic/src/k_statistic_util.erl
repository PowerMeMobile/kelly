-module(k_statistic_util).

-export([
	msg_stats_file_path/1,
	gtw_stats_file_path/1,
	status_stats_file_path/1,
	incoming_msg_stats_file_path/1,

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

-spec status_stats_file_path(string()) -> string().
status_stats_file_path(Filename) ->
	{ok, CWD} = file:get_cwd(),
	Path = lists:flatten(io_lib:format("data/time-slices.d/status-stats/~s", [Filename])),
	filename:join(CWD, Path).

-spec incoming_msg_stats_file_path(string()) -> string().
incoming_msg_stats_file_path(Filename) ->
	{ok, CWD} = file:get_cwd(),
	Path = lists:flatten(io_lib:format("data/time-slices.d/incoming-msg-stats/~s", [Filename])),
	filename:join(CWD, Path).

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
