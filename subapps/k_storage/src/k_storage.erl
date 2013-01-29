-module(k_storage).

-export([
	set_mt_req_info/1,
	set_mt_resp_info/1,
	set_mt_dlr_info/1,

	get_mt_msg_info/2,
	get_mt_msg_info/3,

	set_mo_msg_info/1,
	get_mo_msg_info/2
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/customer.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(#req_info{}) -> ok | {error, reason()}.
set_mt_req_info(ReqInfo = #req_info{}) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mt_req_info(ReqInfo).

-spec set_mt_resp_info(#resp_info{}) -> ok | {error, reason()}.
set_mt_resp_info(RespInfo = #resp_info{}) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mt_resp_info(RespInfo).

-spec set_mt_dlr_info(#dlr_info{}) -> ok | {error, reason()}.
set_mt_dlr_info(DlrInfo = #dlr_info{}) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mt_dlr_info(DlrInfo).

-spec get_mt_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(GatewayId, OutMsgId) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:get_mt_msg_info(GatewayId, OutMsgId).

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:get_mt_msg_info(CustomerId, ClientType, InMsgId).

-spec set_mo_msg_info(#msg_info{}) -> ok | {error, reason()}.
set_mo_msg_info(MsgInfo = #msg_info{}) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mo_msg_info(MsgInfo).

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:get_mo_msg_info(GatewayId, InMsgId).
