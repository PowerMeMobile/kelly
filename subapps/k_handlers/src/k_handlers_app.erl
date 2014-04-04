-module(k_handlers_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("alley_common/include/application_spec.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    k_handlers_sup:start_link().

stop(_State) ->
    ok.
