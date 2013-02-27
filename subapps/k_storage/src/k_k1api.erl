-module(k_k1api).

-export([
	link_sms_request_id_to_msg_ids/5,
	get_msg_ids_by_sms_request_id/4
]).

-include_lib("alley_dto/include/addr.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec link_sms_request_id_to_msg_ids(binary(), binary(), #addr{}, binary(), [any()]) -> ok | {error, reason()}.
link_sms_request_id_to_msg_ids(CustomerId, UserId, SrcAddr, SmsRequestId, MessageIDs) ->
	Selector = 	{
		'customer_id' , CustomerId,
		'user_id'     , UserId,
		'src_addr'    , k_storage_utils:addr_to_doc(SrcAddr),
		'req_id'      , SmsRequestId
	},
	Modifier = {
		'$set' , {
			'customer_id' , CustomerId,
			'user_id'     , UserId,
			'src_addr'    , k_storage_utils:addr_to_doc(SrcAddr),
			'req_id'      , SmsRequestId,
			'msg_ids'     , [
				{customer_id, CID, client_type, ClientType, msg_id, MID} || {CID, ClientType, MID} <- MessageIDs
			]
		}
	},
	mongodb_storage:upsert(k_static_storage, k1api_sms_request_id_to_msg_ids, Selector, Modifier).

-spec get_msg_ids_by_sms_request_id(binary(), binary(), #addr{}, binary()) ->
	{ok, [{binary(), k1api, binary()}]} | {error, reason()}.
get_msg_ids_by_sms_request_id(CustomerId, UserId, SrcAddr, SmsRequestId) ->
	Selector = {
		'customer_id' , CustomerId,
		'user_id'     , UserId,
		'src_addr'    , k_storage_utils:addr_to_doc(SrcAddr),
		'req_id'      , SmsRequestId
	},
	case mongodb_storage:find_one(k_static_storage, k1api_sms_request_id_to_msg_ids, Selector) of
		{ok, Doc} ->
			MsgIds = bsondoc:at(msg_ids, Doc),
			{ok, [{CId, Client, MId} || {_, CId, _, Client, _, MId} <- MsgIds]};
		Error ->
			Error
	end.
