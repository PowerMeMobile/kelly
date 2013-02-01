-module(k_delivery_mode_storage).

-export([
	set_mt_req_info/1,
	set_mt_resp_info/1,
	set_mt_dlr_info/1,

	get_mt_msg_info/3,
	get_mt_msg_info/2,

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
		'ct' , ClientType,
		'imi', InMsgId
	},
	Modifier = {
		'$set', {
			'ct'  , ClientType,
			'ci'  , CustomerId,
			'imi' , InMsgId,
			'gi'  , GatewayId,
			't'   , Type,
			'e'   , Encoding,
			'b'   , Message,
			'sa'  , k_storage_utils:addr_to_doc(SrcAddr),
			'da'  , k_storage_utils:addr_to_doc(DstAddr),
			'rd'  , RegDlr,
			'rqt' , ReqTime
		}
	},
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

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
		'ci'  , CustomerId,
		'ct'  , ClientType,
		'imi' , InMsgId
	},
	Modifier = {
		'$set', {
			'ct'  , ClientType,
			'ci'  , CustomerId,
			'imi' , InMsgId,
			'gi'  , GatewayId,
			'omi' , OutMsgId,
			'rpt' , RespTime,
			'rps' , RespStatus
		}
	},
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_dlr_info(#dlr_info{}) -> ok | {error, reason()}.
set_mt_dlr_info(#dlr_info{
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	dlr_time = DlrTime,
	dlr_status = DlrStatus
}) ->
	Selector = {
		'gi'  , GatewayId,
		'omi' , OutMsgId
	},
	Modifier = {
		'$set', {
			'gi'  , GatewayId,
			'omi' , OutMsgId,
			'dt'  , DlrTime,
			'ds'  , DlrStatus
		}
	},
	Command = {
		'findandmodify' , <<"mt_messages">>,
		'query'  , Selector,
		'update' , Modifier
	},
	Command = {
		'findandmodify' , <<"mt_messages">>,
		'query'  , Selector,
		'update' , Modifier
	},

	case mongodb_storage:command(k_prev_dynamic_storage, Command) of
		{ok, {value, _, lastErrorObject, {updatedExisting, true, n, 1}, ok, _}} ->
			ok;
		{ok, {value, undefined, ok, _}} ->
			mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier)
	end.

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = {
		'ci'  , CustomerId,
		'ct'  , ClientType,
		'imi' , InMsgId,
		'rqt', {'$exists', true}
	},
	case mongodb_storage:find_one(k_curr_dynamic_storage, mt_messages, Selector) of
		{ok, Doc} ->
			{ok, doc_to_mt_msg_info(Doc)};
		{error, no_entry} ->
			case mongodb_storage:find_one(k_prev_dynamic_storage, mt_messages, Selector) of
				{ok, Doc} ->
					{ok, doc_to_mt_msg_info(Doc)};
				Error ->
					Error
			end;
		Error ->
			Error
	end.

-spec get_mt_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(GatewayId, OutMsgId) ->
	Selector = {
		'gi'  , GatewayId,
		'omi' , OutMsgId,
		'rqt' , {'$exists', true}
	},
	case mongodb_storage:find_one(k_curr_dynamic_storage, mt_messages, Selector) of
		{ok, Doc} ->
			{ok, doc_to_mt_msg_info(Doc)};
		{error, no_entry} ->
			case mongodb_storage:find_one(k_prev_dynamic_storage, mt_messages, Selector) of
				{ok, Doc} ->
					{ok, doc_to_mt_msg_info(Doc)};
				Error ->
					Error
			end;
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
		'gi'  , GatewayId,
		'imi' , InMsgId
	},
	Modifier = {
		'$set' , {
			'imi' , InMsgId,
			'gi'  , GatewayId,
			'ci'  , CustomerId,
			't'   , Type,
			'e'   , Encoding,
			'b'   , MessageBody,
			'sa'  , k_storage_utils:addr_to_doc(SrcAddr),
			'da'  , k_storage_utils:addr_to_doc(DstAddr),
			'rd'  , RegDlr,
			'rqt' , ReqTime
		}
	},
	mongodb_storage:upsert(k_curr_dynamic_storage, mo_messages, Selector, Modifier).

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	Selector = {
		gi  , GatewayId,
		imi , InMsgId
	},
	case mongodb_storage:find_one(k_curr_dynamic_storage, mo_messages, Selector) of
		{ok, Doc} ->
			{ok, doc_to_mo_msg_info(Doc)};
		{error, no_entry} ->
			case mongodb_storage:find_one(k_prev_dynamic_storage, mo_messages, Selector) of
				{ok, Doc} ->
					{ok, doc_to_mo_msg_info(Doc)};
				Error ->
					Error
			end;
		Error ->
			Error
	end.

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_mt_msg_info(Doc) ->
	SrcAddrDoc = bson:at(sa, Doc),
	DstAddrDoc = bson:at(da, Doc),
	#msg_info{
		client_type = bson:at(ct, Doc),
		customer_id = bson:at(ci, Doc),
		in_msg_id = bson:at(imi, Doc),
		gateway_id = bson:at(gi, Doc),
		out_msg_id = bson:at(omi, Doc),
		type = bson:at(t, Doc),
		encoding = bson:at(e, Doc),
		body = bson:at(b, Doc),
		src_addr = k_storage_utils:doc_to_addr(SrcAddrDoc),
		dst_addr = k_storage_utils:doc_to_addr(DstAddrDoc),
		reg_dlr = bson:at(rd, Doc),
		req_time = bson:at(rqt, Doc),
		resp_time = bson:at(rpt, Doc),
		resp_status = bson:at(rps, Doc),
		dlr_time = bson:at(dt, Doc),
		dlr_status = bson:at(ds, Doc)
	}.

doc_to_mo_msg_info(Doc) ->
	SrcAddrDoc = bson:at(sa, Doc),
	DstAddrDoc = bson:at(da, Doc),
	#msg_info{
		customer_id = bson:at(ci, Doc),
		in_msg_id = bson:at(imi, Doc),
		gateway_id = bson:at(gi, Doc),
		type = bson:at(t, Doc),
		encoding = bson:at(e, Doc),
		body = bson:at(b, Doc),
		src_addr = k_storage_utils:doc_to_addr(SrcAddrDoc),
		dst_addr = k_storage_utils:doc_to_addr(DstAddrDoc),
		reg_dlr = bson:at(rd, Doc),
		req_time = bson:at(rqt, Doc)
	}.
