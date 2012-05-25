-module(k_mb_amqp).

-include_lib("k_common/include/logging.hrl").

-export([
    call/3
    ]).

-spec call(binary(), term(), integer()) -> ok.
call(QName, Item, TimeOut) ->
	k_mb_amqp_fw_srv:send(QName, Item, TimeOut).
