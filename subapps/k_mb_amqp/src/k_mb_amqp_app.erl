-module(k_mb_amqp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ok = application:set_env(k_mb_amqp, reply_to, <<"pmm.kelly.mailbox_reply">>),
	k_mb_amqp_sup:start_link().

stop(_State) ->
    ok.
