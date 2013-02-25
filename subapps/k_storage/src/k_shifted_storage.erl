-module(k_shifted_storage).

-export([
	get_mt_msg_info/3,
	get_mo_msg_info/2
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/customer.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = {
		'ci'  , CustomerId,
		'ct'  , ClientType,
		'imi' , InMsgId,
		'rqt' , {'$exists', true}
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	case StorageMode:get_mt_msg_info(Selector) of
		{ok, Doc} ->
			{ok, k_storage_utils:doc_to_mt_msg_info(Doc)};
		Error ->
			Error
	end.

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	Selector = {
		'gi'  , GatewayId,
		'imi' , InMsgId
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	case StorageMode:get_mo_msg_info(Selector) of
		{ok, Doc} ->
			{ok, k_storage_utils:doc_to_mo_msg_info(Doc)};
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

