-module(k_common_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include("application.hrl").
-include_lib("alley_common/include/application_spec.hrl").

-record(state, {}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	k_common_sup:start_link().

stop(_State = #state{}) ->
	ok.
