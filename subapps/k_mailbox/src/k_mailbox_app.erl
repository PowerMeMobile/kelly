-module(k_mailbox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	k_mb_config:init(),
	k_mailbox_sup:start_link().

stop(_State) ->
	ok.
