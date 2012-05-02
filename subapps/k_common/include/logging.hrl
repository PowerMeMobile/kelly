-compile([{parse_transform, lager_transform}]).

-ifndef(logging_hrl).
-define(logging_hrl, included).

% -define(log_fixed_width_level(Lvl),
% 		case Lvl of
% 			debug	-> "debug ";
% 			info	-> "info  ";
% 			notice	-> "notice";
% 			warn	-> "warn  ";
% 			error	-> "error ";
% 			crit	-> "crit  ";
% 			alert	-> "alert ";
% 			fatal	-> "fatal "
% 		end).

% -define(log_common(Lvl, Fmt, Args),
% 		io:format("~s [~s] " ++ Fmt ++ " (~s :~p)~n", [
% 			fun() ->
% 				{Y, M, D} = erlang:date(),
% 				{HH, MM, SS} = erlang:time(),
% 				[ case Ch of $\s -> $0; _ -> Ch end || Ch <- lists:flatten(io_lib:format("~4B-~2B-~2B_~2B:~2B:~2B", [Y, M, D, HH, MM, SS])) ]
% 			end (),
% 			?log_fixed_width_level(Lvl)
% 		] ++ Args ++ [ ?FILE, ?LINE ])
% 		% io:format("[~p] [~s]\t" ++ Fmt ++ " [~p:~p]~n",
% 		% 	[
% 		% 		atom_to_list(Lvl),
% 		% 		fun() ->
% 		% 			{Y, M, D} = erlang:date(),
% 		% 			{HH, MM, SS} = erlang:time(),
% 		% 			io_lib:format("~2B-~2B-~2B ~2B:~2B:~2B", [Y, M, D, HH, MM, SS])
% 		% 		end (),
% 		% 		| Args
% 		% 	] ++ [?FILE, ?LINE]
% 		%)
% 	).

% -define(log_debug(Fmt, Args), ?log_common(debug, Fmt, Args)).
% -define(log_info(Fmt, Args), ?log_common(info, Fmt, Args)).
% -define(log_notice(Fmt, Args), ?log_common(notice, Fmt, Args)).
% -define(log_warn(Fmt, Args), ?log_common(warn, Fmt, Args)).
% -define(log_error(Fmt, Args), ?log_common(error, Fmt, Args)).
% -define(log_crit(Fmt, Args), ?log_common(crit, Fmt, Args)).
% -define(log_alert(Fmt, Args), ?log_common(alert, Fmt, Args)).
% -define(log_fatal(Fmt, Args), ?log_common(fatal, Fmt, Args)).

% -define(log_debug(Fmt, Args), lager:log(debug, self(), Fmt, Args)).
% -define(log_info(Fmt, Args), lager:log(info, self(), Fmt, Args)).
% -define(log_notice(Fmt, Args), lager:log(notice, self(), Fmt, Args)).
% -define(log_warn(Fmt, Args), lager:log(warning, self(), Fmt, Args)).
% -define(log_error(Fmt, Args), lager:log(error, self(), Fmt, Args)).
% -define(log_crit(Fmt, Args), lager:log(critical, self(), Fmt, Args)).
% -define(log_alert(Fmt, Args), lager:log(alert, self(), Fmt, Args)).
% -define(log_fatal(Fmt, Args), lager:log(emergency, self(), Fmt, Args)).

-define( log_common(Lvl, Fmt, Args),
		lager:Lvl(Fmt ++  " [~s:~p]",Args ++ [?FILE, ?LINE] )
	).

-define( log_debug(Fmt, Args), ?log_common(debug, Fmt, Args) ).
-define( log_info(Fmt, Args), ?log_common(info, Fmt, Args) ).
-define( log_notice(Fmt, Args), ?log_common(notice, Fmt, Args) ).
-define( log_warn(Fmt, Args), ?log_common(warning, Fmt, Args) ).
-define( log_error(Fmt, Args), ?log_common(error, Fmt, Args) ).
-define( log_crit(Fmt, Args), ?log_common(critical, Fmt, Args) ).
-define( log_alert(Fmt, Args), ?log_common(alert, Fmt, Args) ).
-define( log_fatal(Fmt, Args), ?log_common(emergency, Fmt, Args) ).


-endif. % logging_hrl
