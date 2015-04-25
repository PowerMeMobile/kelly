-module(k_control_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/application_spec.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    k_control_sup:start_link().

stop(_State) ->
    ok.
