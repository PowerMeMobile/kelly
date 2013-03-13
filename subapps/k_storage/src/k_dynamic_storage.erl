-module(k_dynamic_storage).

-export([
	set_mt_req_info/1,
	set_mt_resp_info/1,
	set_mt_dlr_info_and_get_msg_info/1,

	set_mo_msg_info/1
]).

-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/customer.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(#req_info{}) -> ok | {error, reason()}.
set_mt_req_info(#req_info{
	client_type = ClientType,
	customer_id = CustomerId,
	in_msg_id = InMsgId,
	gateway_id = GatewayId,
	type = Type,
	encoding = Encoding,
	body = Message,
	src_addr = SrcAddr,
	dst_addr = DstAddr,
	reg_dlr = RegDlr,
	req_time = ReqTime
}) ->
	Selector = {
		'ci' , CustomerId,
		'ct' , bsondoc:atom_to_binary(ClientType),
		'imi', InMsgId
	},
	Modifier = {
		'$set', {
			'ct' , bsondoc:atom_to_binary(ClientType),
			'ci' , CustomerId,
			'imi', InMsgId,
			'gi' , GatewayId,
			't'  , bsondoc:atom_to_binary(Type),
			'e'  , bsondoc:atom_to_binary(Encoding),
			'b'  , Message,
			'sa' , k_storage_utils:addr_to_doc(SrcAddr),
			'da' , k_storage_utils:addr_to_doc(DstAddr),
			'rd' , RegDlr,
			'rqt', ReqTime
		}
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mt_req_info(Selector, Modifier).

-spec set_mt_resp_info(#resp_info{}) -> ok | {error, reason()}.
set_mt_resp_info(#resp_info{
	client_type = ClientType,
	customer_id = CustomerId,
	in_msg_id = InMsgId,
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	resp_time = RespTime,
	resp_status = RespStatus
}) ->
	Selector = {
		'ci' , CustomerId,
		'ct' , bsondoc:atom_to_binary(ClientType),
		'imi', InMsgId
	},
	Modifier = {
		'$set', {
			'ct' , bsondoc:atom_to_binary(ClientType),
			'ci' , CustomerId,
			'imi', InMsgId,
			'gi' , GatewayId,
			'omi', OutMsgId,
			'rpt', RespTime,
			'rps', bsondoc:atom_to_binary(RespStatus)
		}
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mt_resp_info(Selector, Modifier).

-spec set_mt_dlr_info_and_get_msg_info(#dlr_info{}) -> {ok, #msg_info{}} | {error, reason()}.
set_mt_dlr_info_and_get_msg_info(#dlr_info{
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	dlr_time = DlrTime,
	dlr_status = DlrStatus
}) ->
	Selector = {
		'gi' , GatewayId,
		'omi', OutMsgId,
		'rqt', {'$exists', true}
	},
	Modifier = {
		'$set', {
			'gi' , GatewayId,
			'omi', OutMsgId,
			'dt' , DlrTime,
			'ds' , bsondoc:atom_to_binary(DlrStatus)
		}
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	case StorageMode:set_mt_dlr_info_and_get_msg_info(Selector, Modifier) of
		{ok, Doc} ->
			{ok, k_storage_utils:doc_to_mt_msg_info(Doc)};
		Error ->
			Error
	end.

-spec set_mo_msg_info(#msg_info{}) -> ok | {error, reason()}.
set_mo_msg_info(MsgInfo = #msg_info{}) ->
	InMsgId = MsgInfo#msg_info.in_msg_id,
	GatewayId = MsgInfo#msg_info.gateway_id,
	CustomerId = MsgInfo#msg_info.customer_id,
	Type = MsgInfo#msg_info.type,
	Encoding = MsgInfo#msg_info.encoding,
	MessageBody = MsgInfo#msg_info.body,
	SrcAddr = MsgInfo#msg_info.src_addr,
	DstAddr = MsgInfo#msg_info.dst_addr,
	RegDlr = MsgInfo#msg_info.reg_dlr,
	ReqTime = MsgInfo#msg_info.req_time,

	Selector = {
		'gi' , GatewayId,
		'imi', InMsgId
	},
	Modifier = {
		'$set' , {
			'imi', InMsgId,
			'gi' , GatewayId,
			'ci' , CustomerId,
			't'  , bsondoc:atom_to_binary(Type),
			'e'  , bsondoc:atom_to_binary(Encoding),
			'b'  , MessageBody,
			'sa' , k_storage_utils:addr_to_doc(SrcAddr),
			'da' , k_storage_utils:addr_to_doc(DstAddr),
			'rd' , RegDlr,
			'rqt', ReqTime
		}
	},
	{ok, StorageMode} = k_storage_manager:get_storage_mode(),
	StorageMode:set_mo_msg_info(Selector, Modifier).
