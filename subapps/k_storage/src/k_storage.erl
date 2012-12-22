-module(k_storage).

-export([
	set_mt_req_info/1,
	set_mt_resp_info/1,
	set_mt_dlr_info/1,

	get_mt_msg_info/2,
	get_mt_msg_info/3,

	set_mo_msg_info/1,
	get_mo_msg_info/2,

	link_sms_request_id_to_msg_ids/5,
	get_msg_ids_by_sms_request_id/4
]).

-export([
	doc_to_addr/1
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_common/include/gateway.hrl").
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
	Selector = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	Plist = [
		{client_type, ClientType},
		{customer_id, CustomerId},
		{in_msg_id, InMsgId},
		{gateway_id, GatewayId},
		{type, Type},
		{encoding, Encoding},
		{body, Message},
		{src_addr, addr_to_doc(SrcAddr)},
		{dst_addr, addr_to_doc(DstAddr)},
		{reg_dlr, RegDlr},
		{req_time, ReqTime}
	],
	mongodb_storage:upsert(mt_messages, Selector, Plist).

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
	Selector = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	Plist = [
		{client_type, ClientType},
		{customer_id, CustomerId},
		{in_msg_id, InMsgId},
		{gateway_id, GatewayId},
		{out_msg_id, OutMsgId},
		{resp_time, RespTime},
		{resp_status, RespStatus}
	],
	mongodb_storage:upsert(mt_messages, Selector, Plist).

-spec set_mt_dlr_info(#dlr_info{}) -> ok | {error, reason()}.
set_mt_dlr_info(#dlr_info{
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	dlr_time = DlrTime,
	dlr_status = DlrStatus
}) ->
	Selector = [{gateway_id, GatewayId}, {out_msg_id, OutMsgId}],
	Plist = [
		{gateway_id, GatewayId},
		{out_msg_id, OutMsgId},
		{dlr_time, DlrTime},
		{dlr_status, DlrStatus}
	],
	mongodb_storage:upsert(mt_messages, Selector, Plist).

-spec get_mt_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(GatewayId, OutMsgId) ->
	Selector = [{gateway_id, GatewayId}, {out_msg_id, OutMsgId}],
	case mongodb_storage:find_one(mt_messages, Selector) of
		{ok, Plist} ->
			{ok, plist_to_msg_info(Plist)};
		Error ->
			Error
	end.

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	case mongodb_storage:find_one(mt_messages, Selector) of
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

	Selector = [{gateway_id, GatewayId}, {in_msg_id, InMsgId}],
	Plist = [
		{in_msg_id, InMsgId},
		{gateway_id, GatewayId},
		{customer_id, CustomerId},
		{type, Type},
		{encoding, Encoding},
		{body, MessageBody},
		{src_addr, addr_to_doc(SrcAddr)},
		{dst_addr, addr_to_doc(DstAddr)},
		{reg_dlr, RegDlr},
		{req_time, ReqTime}
	],
	mongodb_storage:upsert(mo_messages, Selector, Plist).

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	Selector = [{gateway_id, GatewayId}, {in_msg_id, InMsgId}],
	case mongodb_storage:find_one(mo_messages, Selector) of
		{ok, Plist} ->
			SrcAddrDoc = proplists:get_value(src_addr, Plist),
			DstAddrDoc = proplists:get_value(dst_addr, Plist),
			MsgInfo = #msg_info{
				customer_id = proplists:get_value(customer_id, Plist),
				in_msg_id = proplists:get_value(in_msg_id, Plist),
				gateway_id = proplists:get_value(gateway_id, Plist),
				type = proplists:get_value(type, Plist),
				encoding = proplists:get_value(encoding, Plist),
				body = proplists:get_value(body, Plist),
				src_addr = doc_to_addr(SrcAddrDoc),
				dst_addr = doc_to_addr(DstAddrDoc),
				reg_dlr = proplists:get_value(reg_dlr, Plist),
				req_time = proplists:get_value(req_time, Plist)
			},
			{ok, MsgInfo};
		Error ->
			Error
	end.

-spec link_sms_request_id_to_msg_ids(binary(), binary(), #addr{}, binary(), [any()]) -> ok | {error, reason()}.
link_sms_request_id_to_msg_ids(CustomerId, UserId, SrcAddr, SmsRequestId, MessageIDs) ->
	Selector = 	[
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SrcAddr)},
		{req_id, SmsRequestId}
	],
	Plist = [
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SrcAddr)},
		{req_id, SmsRequestId},
		{msg_ids, [{customer_id, CId, client_type, Client, msg_id, MId} || {CId, Client, MId} <- MessageIDs]}
	],
	mongodb_storage:upsert(k1api_sms_request_id_to_msg_ids, Selector, Plist).

-spec get_msg_ids_by_sms_request_id(binary(), binary(), #addr{}, binary()) ->
	{ok, [{binary(), k1api, binary()}]} | {error, reason()}.
get_msg_ids_by_sms_request_id(CustomerId, UserId, SrcAddr, SmsRequestId) ->
	Selector = [
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SrcAddr)},
		{req_id, SmsRequestId}
	],
	case mongodb_storage:find_one(k1api_sms_request_id_to_msg_ids, Selector) of
		{ok, Plist} ->
			MsgIdsDoc = proplists:get_value(msg_ids, Plist),
			{ok, [{CId, Client, MId} || {_,CId,_,Client,_, MId} <- MsgIdsDoc]};
		Error ->
			Error
	end.

%% ===================================================================
%% Internals
%% ===================================================================

plist_to_msg_info(Plist) ->
	SrcAddrDoc = proplists:get_value(src_addr, Plist),
	DstAddrDoc = proplists:get_value(dst_addr, Plist),
	#msg_info{
		client_type = proplists:get_value(client_type, Plist),
		customer_id = proplists:get_value(customer_id, Plist),
		in_msg_id = proplists:get_value(in_msg_id, Plist),
		gateway_id = proplists:get_value(gateway_id, Plist),
		out_msg_id = proplists:get_value(out_msg_id, Plist),
		type = proplists:get_value(type, Plist),
		encoding = proplists:get_value(encoding, Plist),
		body = proplists:get_value(body, Plist),
		src_addr = doc_to_addr(SrcAddrDoc),
		dst_addr = doc_to_addr(DstAddrDoc),
		reg_dlr = proplists:get_value(reg_dlr, Plist),
		req_time = proplists:get_value(req_time, Plist),
		resp_time = proplists:get_value(resp_time, Plist),
		resp_status = proplists:get_value(resp_status, Plist),
		dlr_time = proplists:get_value(dlr_time, Plist),
		dlr_status = proplists:get_value(dlr_status, Plist)
	}.

addr_to_doc(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
	{addr, Addr, ton, Ton, npi, Npi};
addr_to_doc(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
	{addr, Addr, ton, Ton, npi, Npi, ref_num, RefNum}.

-spec doc_to_addr
	({addr, binary(), ton, integer(), npi, integer()}) ->
		#addr{};
	({addr, binary(), ton, integer(), npi, integer(), ref_num, integer()}) ->
		#addr{}.
doc_to_addr({addr, Addr, ton, Ton, npi, Npi}) ->
	#addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	};
doc_to_addr({addr, Addr, ton, Ton, npi, Npi, ref_num, RefNum}) ->
	#addr{
		addr = Addr,
		ton = Ton,
		npi = Npi,
		ref_num = RefNum
	}.
