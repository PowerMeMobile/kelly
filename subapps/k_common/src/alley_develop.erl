-module(alley_develop).

-export([init/0]).

init() ->
	 lager:set_loglevel(lager_console_backend, debug).
