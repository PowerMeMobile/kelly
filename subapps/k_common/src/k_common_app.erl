-module(k_common_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Init
-export([set_debug_level/0]).

-include("application.hrl").
-include("application_spec.hrl").


-record(state, {}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	k_common_sup:start_link().

stop(_State = #state{}) ->
	ok.

%% ===================================================================
%% Init API
%% ===================================================================

set_debug_level() ->
	 lager:set_loglevel(lager_console_backend, debug).
