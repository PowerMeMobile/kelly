-compile([{parse_transform, lager_transform}]).

-ifndef(logging_hrl).
-define(logging_hrl, included).

-define(log_common(Lvl, Fmt, Args),
	lager:Lvl(Fmt ++  " [~s:~p]", Args ++ [filename:basename(?FILE), ?LINE])
).

-define(log_debug(Fmt, Args), ?log_common(debug, Fmt, Args)).
-define(log_info(Fmt, Args), ?log_common(info, Fmt, Args)).
-define(log_notice(Fmt, Args), ?log_common(notice, Fmt, Args)).
-define(log_warn(Fmt, Args), ?log_common(warning, Fmt, Args)).
-define(log_error(Fmt, Args), ?log_common(error, Fmt, Args)).
-define(log_crit(Fmt, Args), ?log_common(critical, Fmt, Args)).
-define(log_alert(Fmt, Args), ?log_common(alert, Fmt, Args)).
-define(log_fatal(Fmt, Args), ?log_common(emergency, Fmt, Args)).

-endif. % logging_hrl
