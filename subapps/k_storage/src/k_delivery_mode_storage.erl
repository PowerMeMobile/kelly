-module(k_delivery_mode_storage).

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
	Selector = [{ci, CustomerId}, {ct, ClientType}, {imi, InMsgId}],
	Plist = [
		{ct, ClientType},
		{ci, CustomerId},
		{imi, InMsgId},
		{gi, GatewayId},
		{t, Type},
		{e, Encoding},
		{b, Message},
		{sa, k_storage_utils:addr_to_doc(SrcAddr)},
		{da, k_storage_utils:addr_to_doc(DstAddr)},
		{rd, RegDlr},
		{rqt, ReqTime}
	],
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Plist).

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
	Selector = [{ci, CustomerId}, {ct, ClientType}, {imi, InMsgId}],
	Plist = [
		{ct, ClientType},
		{ci, CustomerId},
		{imi, InMsgId},
		{gi, GatewayId},
		{omi, OutMsgId},
		{rpt, RespTime},
		{rps, RespStatus}
	],
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Plist).

-spec set_mt_dlr_info(#dlr_info{}) -> ok | {error, reason()}.
set_mt_dlr_info(#dlr_info{
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	dlr_time = DlrTime,
	dlr_status = DlrStatus
}) ->
	Selector = [{gi, GatewayId}, {omi, OutMsgId}],
	Plist = [
		{gi, GatewayId},
		{omi, OutMsgId},
		{dt, DlrTime},
		{ds, DlrStatus}
	],
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Plist).

-spec get_mt_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(GatewayId, OutMsgId) ->
	Selector = [{gi, GatewayId}, {omi, OutMsgId}],
	case mongodb_storage:find_one(k_curr_dynamic_storage, mt_messages, Selector) of
		{ok, Plist} ->
			{ok, plist_to_msg_info(Plist)};
		Error ->
			Error
	end.

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = [{ci, CustomerId}, {ct, ClientType}, {imi, InMsgId}],
	case mongodb_storage:find_one(k_curr_dynamic_storage, mt_messages, Selector) of
		{ok, Plist} ->
			{ok, plist_to_msg_info(Plist)};
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

	Selector = [{gi, GatewayId}, {imi, InMsgId}],
	Plist = [
		{imi, InMsgId},
		{gi, GatewayId},
		{ci, CustomerId},
		{t, Type},
		{e, Encoding},
		{b, MessageBody},
		{sa, k_storage_utils:addr_to_doc(SrcAddr)},
		{da, k_storage_utils:addr_to_doc(DstAddr)},
		{rd, RegDlr},
		{rqt, ReqTime}
	],
	mongodb_storage:upsert(k_curr_dynamic_storage, mo_messages, Selector, Plist).

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	Selector = [{gi, GatewayId}, {imi, InMsgId}],
	case mongodb_storage:find_one(k_curr_dynamic_storage, mo_messages, Selector) of
		{ok, Plist} ->
			SrcAddrDoc = proplists:get_value(sa, Plist),
			DstAddrDoc = proplists:get_value(da, Plist),
			MsgInfo = #msg_info{
				customer_id = proplists:get_value(ci, Plist),
				in_msg_id = proplists:get_value(imi, Plist),
				gateway_id = proplists:get_value(gi, Plist),
				type = proplists:get_value(t, Plist),
				encoding = proplists:get_value(e, Plist),
				body = proplists:get_value(b, Plist),
				src_addr = k_storage_utils:doc_to_addr(SrcAddrDoc),
				dst_addr = k_storage_utils:doc_to_addr(DstAddrDoc),
				reg_dlr = proplists:get_value(rd, Plist),
				req_time = proplists:get_value(rqt, Plist)
			},
			{ok, MsgInfo};
		Error ->
			Error
	end.

%% ===================================================================
%% Internals
%% ===================================================================

plist_to_msg_info(Plist) ->
	SrcAddrDoc = proplists:get_value(sa, Plist),
	DstAddrDoc = proplists:get_value(da, Plist),
	#msg_info{
		client_type = proplists:get_value(ct, Plist),
		customer_id = proplists:get_value(ci, Plist),
		in_msg_id = proplists:get_value(imi, Plist),
		gateway_id = proplists:get_value(gi, Plist),
		out_msg_id = proplists:get_value(omi, Plist),
		type = proplists:get_value(t, Plist),
		encoding = proplists:get_value(e, Plist),
		body = proplists:get_value(b, Plist),
		src_addr = k_storage_utils:doc_to_addr(SrcAddrDoc),
		dst_addr = k_storage_utils:doc_to_addr(DstAddrDoc),
		reg_dlr = proplists:get_value(rd, Plist),
		req_time = proplists:get_value(rqt, Plist),
		resp_time = proplists:get_value(rpt, Plist),
		resp_status = proplists:get_value(rps, Plist),
		dlr_time = proplists:get_value(dt, Plist),
		dlr_status = proplists:get_value(ds, Plist)
	}.
