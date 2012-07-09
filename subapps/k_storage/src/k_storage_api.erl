-module(k_storage_api).

-export([
	set_msg_info/2,
	get_msg_info/1,
	set_msg_status/2,
	get_msg_status/1,
	map_input_id_to_output_id/2,
	get_output_id_by_input_id/1,
	map_output_id_to_input_id/2,
	get_input_id_by_output_id/1,

	utc_unix_epoch/0
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/storages.hrl").

%% Public APIs

-spec set_msg_info(msg_id(), #msg_info{}) -> ok | {error, any()}.
set_msg_info(InputId, MsgInfo = #msg_info{}) ->
	gen_server:call(msg_info, {set, InputId, MsgInfo}, infinity).

-spec get_msg_info(msg_id()) -> {ok, #msg_info{}} | {error, any()}.
get_msg_info(InputId) ->
	gen_server:call(msg_info, {get, InputId}, infinity).

-spec set_msg_status(msg_id(), #msg_status{}) -> ok | {error, any()}.
set_msg_status(InputId, MsgStatus) ->
	gen_server:call(msg_status, {set, InputId, MsgStatus}, infinity).

-spec get_msg_status(msg_id()) -> {ok, #msg_status{}} | {error, any()}.
get_msg_status(InputId) ->
	gen_server:call(msg_status, {get, InputId}, infinity).

-spec map_input_id_to_output_id(msg_id(), msg_id()) -> ok | {error, any()}.
map_input_id_to_output_id(InputId, OutputId) ->
	gen_server:call(in_to_out, {set, InputId, OutputId}, infinity).

-spec get_output_id_by_input_id(msg_id()) -> {ok, msg_id()} | {error, any()}.
get_output_id_by_input_id(InputId) ->
	gen_server:call(in_to_out, {get, InputId}, infinity).

-spec map_output_id_to_input_id(msg_id(), msg_id()) -> ok | {error, any()}.
map_output_id_to_input_id(OutputId, InputId) ->
	gen_server:call(out_to_in, {set, OutputId, InputId}, infinity).

-spec get_input_id_by_output_id(msg_id()) -> {ok, msg_id()} | {error, any()}.
get_input_id_by_output_id(OutputId) ->
	gen_server:call(out_to_in, {get, OutputId}, infinity).

-spec utc_unix_epoch() -> integer().
utc_unix_epoch() ->
	k_storage_util:utc_unix_epoch().
