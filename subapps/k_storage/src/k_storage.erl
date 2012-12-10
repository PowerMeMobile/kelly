-module(k_storage).

-export([
	set_outgoing_msg_info/1,
	get_outgoing_msg_info/2,
	get_outgoing_msg_info/3,

	set_incoming_msg_info/2,
	get_incoming_msg_info/1,

	link_sms_request_id_to_msg_ids/2,
	get_msg_ids_by_sms_request_id/1
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
	Selectors = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
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
	mongodb_storage:upsert(outgoing_messages, Selectors, Plist);
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
	Selectors = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	Plist = [
		{client_type, ClientType},
		{customer_id, CustomerId},
		{in_msg_id, InMsgId},
		{gateway_id, GatewayId},
		{out_msg_id, OutMsgId},
		{resp_time, RespTime},
		{resp_status, RespStatus}
	],
	mongodb_storage:upsert(outgoing_messages, Selectors, Plist);
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
	Selectors = [{gateway_id, GatewayId}, {out_msg_id, OutMsgId}],
	Plist = [
		{gateway_id, GatewayId},
		{out_msg_id, OutMsgId},
		{dlr_time, DlrTime},
		{dlr_status, DlrStatus}
	],
	mongodb_storage:upsert(outgoing_messages, Selectors, Plist).

addr_to_doc(#full_addr{addr = Addr, ton = Ton, npi = Npi}) ->
	{addr, Addr, ton, Ton, npi, Npi};
addr_to_doc(#full_addr_ref_num{full_addr = FullAddr, ref_num = RefNum}) ->
	{addr, addr_to_doc(FullAddr), ref_num, RefNum}.

doc_to_addr({addr, Addr, ton, Ton, npi, Npi}) ->
	#full_addr{addr = Addr, ton = Ton, npi = Npi};
doc_to_addr({addr, Addr, ref_num, RefNum}) ->
	#full_addr_ref_num{full_addr = doc_to_addr(Addr), ref_num = RefNum}.

-spec get_outgoing_msg_info(gateway_id(), msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_outgoing_msg_info(GatewayId, OutMsgId) ->
	Selectors = [{gateway_id, GatewayId}, {out_msg_id, OutMsgId}],
	case mongodb_storage:find_one(outgoing_messages, Selectors) of
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
	Selectors = [{customer_id, CustomerId}, {client_type, ClientType}, {in_msg_id, InMsgId}],
	case mongodb_storage:find_one(outgoing_messages, Selectors) of
		{ok, Plist} ->
			MsgInfo = record_info:proplist_to_record(Plist, msg_info, ?MODULE),
			SrcAddr = doc_to_addr(MsgInfo#msg_info.src_addr),
			DstAddr = doc_to_addr(MsgInfo#msg_info.dst_addr),
			{ok, MsgInfo#msg_info{src_addr = SrcAddr, dst_addr = DstAddr}};
		Error ->
			Error
	end.

%% ===================================================================
%% Use kv_storage
%% ===================================================================

-spec set_incoming_msg_info(msg_id(), #msg_info{}) -> ok | {error, reason()}.
set_incoming_msg_info(OutputId, MsgInfo = #msg_info{}) ->
	gen_server:call(incoming_msg_info, {set, OutputId, MsgInfo}, infinity).

-spec get_incoming_msg_info(msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_incoming_msg_info(OutputId) ->
	gen_server:call(incoming_msg_info, {get, OutputId}, infinity).

-spec link_sms_request_id_to_msg_ids(binary(), [binary()]) -> ok | {error, reason()}.
link_sms_request_id_to_msg_ids(SmsRequestID, MessageIDs) ->
	gen_server:call(k1api_sms_request_id_to_msg_ids, {set, SmsRequestID, MessageIDs}).

-spec get_msg_ids_by_sms_request_id(binary()) -> {ok, [binary()]} | {error, reason()}.
get_msg_ids_by_sms_request_id(SmsRequestID) ->
	gen_server:call(k1api_sms_request_id_to_msg_ids, {get, SmsRequestID}).
