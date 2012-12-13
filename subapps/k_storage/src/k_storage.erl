-module(k_storage).

-export([
	set_outgoing_msg_info/1,
	get_outgoing_msg_info/2,
	get_outgoing_msg_info/3,

	set_incoming_msg_info/1,
	get_incoming_msg_info/2,

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

-include_lib("record_info/include/record_info.hrl").
-export_record_info([msg_info]).

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec set_outgoing_msg_info(#msg_info{}) -> ok | {error, reason()}.
set_outgoing_msg_info(#msg_info{
	client_type = ClientType,
	customer_id = CustomerId,
	in_msg_id = InMsgId,
	gateway_id = GatewayId,
	out_msg_id = undefined,
	type = Type,
	encoding = Encoding,
	body = Message,
	src_addr = SrcAddr,
	dst_addr = DstAddr,
	reg_dlr = RegDlr,
	req_time = ReqTime,
	resp_time = undefined,
	resp_status = undefined,
	dlr_time = undefined,
	dlr_status = undefined
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
	mongodb_storage:upsert(outgoing_messages, Selector, Plist);
set_outgoing_msg_info(#msg_info{
	client_type = ClientType,
	customer_id = CustomerId,
	in_msg_id = InMsgId,
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	type = undefined,
	encoding = undefined,
	body = undefined,
	src_addr = undefined,
	dst_addr = undefined,
	reg_dlr = undefined,
	req_time = undefined,
	resp_time = RespTime,
	resp_status = RespStatus,
	dlr_time = undefined,
	dlr_status = undefined
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
	mongodb_storage:upsert(outgoing_messages, Selector, Plist);
set_outgoing_msg_info(#msg_info{
	client_type = undefined,
	customer_id = undefined,
	in_msg_id = undefined,
	gateway_id = GatewayId,
	out_msg_id = OutMsgId,
	type = undefined,
	encoding = undefined,
	body = undefined,
	src_addr = undefined,
	dst_addr = undefined,
	reg_dlr = undefined,
	req_time = undefined,
	resp_time = undefined,
	resp_status = undefined,
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
	mongodb_storage:upsert(outgoing_messages, Selector, Plist).

-spec get_outgoing_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_outgoing_msg_info(GatewayId, OutMsgId) ->
	Selector = [{gateway_id, GatewayId}, {out_msg_id, OutMsgId}],
	case mongodb_storage:find_one(outgoing_messages, Selector) of
		{ok, Plist} ->
			MsgInfo = record_info:proplist_to_record(Plist, msg_info, ?MODULE),
			SrcAddr = doc_to_addr(MsgInfo#msg_info.src_addr),
			DstAddr = doc_to_addr(MsgInfo#msg_info.dst_addr),
			{ok, MsgInfo#msg_info{src_addr = SrcAddr, dst_addr = DstAddr}};
		Error ->
			Error
	end.

-spec get_outgoing_msg_info(customer_id(), atom(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_outgoing_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	case mongodb_storage:find_one(outgoing_messages, Selector) of
		{ok, Plist} ->
			MsgInfo = record_info:proplist_to_record(Plist, msg_info, ?MODULE),
			SrcAddr = doc_to_addr(MsgInfo#msg_info.src_addr),
			DstAddr = doc_to_addr(MsgInfo#msg_info.dst_addr),
			{ok, MsgInfo#msg_info{src_addr = SrcAddr, dst_addr = DstAddr}};
		Error ->
			Error
	end.

-spec set_incoming_msg_info(#msg_info{}) -> ok | {error, reason()}.
set_incoming_msg_info(MsgInfo = #msg_info{}) ->
	InMsgId = MsgInfo#msg_info.in_msg_id,
	GatewayId = MsgInfo#msg_info.gateway_id,
	CustomerId = MsgInfo#msg_info.customer_id,
	Type = MsgInfo#msg_info.type,
	Encoding = MsgInfo#msg_info.encoding,
	MessageBody = MsgInfo#msg_info.body,
	SourceAddr = MsgInfo#msg_info.src_addr,
	DestAddr = MsgInfo#msg_info.dst_addr,
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
		{src_addr, addr_to_doc(SourceAddr)},
		{dst_addr, addr_to_doc(DestAddr)},
		{reg_dlr, RegDlr},
		{req_time, ReqTime}
	],
	mongodb_storage:upsert(incoming_messages, Selector, Plist).

-spec get_incoming_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_incoming_msg_info(GatewayId, InMsgId) ->
	Selector = [{gateway_id, GatewayId}, {in_msg_id, InMsgId}],
	case mongodb_storage:find_one(incoming_messages, Selector) of
		{ok, Plist} ->
			SourceAddrDoc = proplists:get_value(src_addr, Plist),
			DestAddrDoc = proplists:get_value(dst_addr, Plist),
			MsgInfo = #msg_info{
				in_msg_id = proplists:get_value(in_msg_id, Plist),
				gateway_id = proplists:get_value(gateway_id, Plist),
				customer_id = proplists:get_value(customer_id, Plist),
				type = proplists:get_value(type, Plist),
				encoding = proplists:get_value(encoding, Plist),
				body = proplists:get_value(body, Plist),
				src_addr = doc_to_addr(SourceAddrDoc),
				dst_addr = doc_to_addr(DestAddrDoc),
				reg_dlr = proplists:get_value(reg_dlr, Plist),
				req_time = proplists:get_value(req_time, Plist)
			},
			{ok, MsgInfo};
		Error ->
			Error
	end.

-spec link_sms_request_id_to_msg_ids(binary(), binary(), #addr{}, binary(), [any()]) -> ok | {error, reason()}.
link_sms_request_id_to_msg_ids(CustomerId, UserId, SourceAddress, SmsRequestId, MessageIDs) ->
	Selector = 	[
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SourceAddress)},
		{req_id, SmsRequestId}
	],
	Plist = [
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SourceAddress)},
		{req_id, SmsRequestId},
		{msg_ids, [{customer_id, CId, client_type, Client, msg_id, MId} || {CId, Client, MId} <- MessageIDs]}
	],
	mongodb_storage:upsert(k1api_sms_request_id_to_msg_ids, Selector, Plist).

-spec get_msg_ids_by_sms_request_id(binary(), binary(), #addr{}, binary()) ->
	{ok, [{binary(), k1api, binary()}]} | {error, reason()}.
get_msg_ids_by_sms_request_id(CustomerId, UserId, SourceAddr, SmsRequestId) ->
	Selector = [
		{customer_id, CustomerId},
		{user_id, UserId},
	  	{src_addr, addr_to_doc(SourceAddr)},
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
