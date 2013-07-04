-module(k_mailbox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	k_mb_config:init(),
	k_mailbox_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
	ok.
