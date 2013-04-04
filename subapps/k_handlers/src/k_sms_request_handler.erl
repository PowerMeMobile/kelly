-module(k_sms_request_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/application.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#just_sms_request_dto{}, Message) of
		{ok, SmsRequest} ->
			process_sms_request(SmsRequest);
		Error ->
			Error
	end.

-spec process_sms_request(#just_sms_request_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_request(SmsRequest = #just_sms_request_dto{client_type = ClientType}) ->
	?log_debug("Got ~p sms request: ~p", [ClientType, SmsRequest]),
	ReqInfos = sms_request_to_req_info_list(SmsRequest),
	case k_utils:safe_foreach(
		fun k_dynamic_storage:set_mt_req_info/1, ReqInfos, ok, {error, '_'}
	) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec sms_request_to_req_info_list(#just_sms_request_dto{}) -> [#req_info{}].
sms_request_to_req_info_list(SmsRequest = #just_sms_request_dto{
	client_type = funnel,
	dest_addrs = {_, DstAddrs},
	message_ids = MsgIds
}) ->
	ReqTime = k_datetime:utc_timestamp(),
	Pairs = zip_addrs_and_ids(DstAddrs, MsgIds),
	[build_req_info(SmsRequest, ReqTime, DstAddr, InMsgId) || {DstAddr, InMsgId} <- Pairs];
sms_request_to_req_info_list(SmsRequest = #just_sms_request_dto{
	client_type = k1api,
	dest_addrs = {_, DstAddrs},
	message_ids = MsgIds
}) ->
	ReqTime = k_datetime:utc_timestamp(),
	Pairs = zip_addrs_and_ids(DstAddrs, MsgIds),
	process_k1api_req(SmsRequest, Pairs),
	[build_req_info(SmsRequest, ReqTime, DstAddr, InMsgId) || {DstAddr, InMsgId} <- Pairs].

build_req_info(#just_sms_request_dto{
	id = ReqId,
	client_type = ClientType,
	customer_id = CustomerId,
	user_id = UserId,
	gateway_id = GatewayId,
	type = Type,
	message = Message,
	encoding = Encoding,
	params = Params,
	source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId) ->
	RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
	#req_info{
		req_id = ReqId,
		client_type = ClientType,
		customer_id = CustomerId,
		user_id = UserId,
		in_msg_id = InMsgId,
		gateway_id = GatewayId,
		type = Type,
		encoding = Encoding,
		body = Message,
		src_addr = SrcAddr,
		dst_addr = DstAddr,
		reg_dlr = RegDlr,
		req_time = ReqTime
	}.

%% Message ids come in ["ID1", "ID2:ID3", "ID4"], where "ID2:ID3" is a multipart message ids.
%% Destination addrs come in ["ADDR1", "ADDR2", "ADDR3"]. The task is to get {ADDRX, IDY} pairs
%% like that [{"ADDR1", "ID1"}, {"ADDR2", "ID2"}, {"ADDR2", "ID3"}, {"ADDR3", "ID4"}].
zip_addrs_and_ids(Addrs, MsgIds) ->
	zip_addrs_and_ids(Addrs, MsgIds, []).

zip_addrs_and_ids([], [], Acc) ->
	Acc;
zip_addrs_and_ids([Addr|Addrs], [MsgId|MsgIds], Acc) ->
	Ids = binary:split(MsgId, <<":">>, [global, trim]),
	Pairs = [{Addr, Id} || Id <- Ids],
	zip_addrs_and_ids(Addrs, MsgIds, Pairs ++ Acc).

-spec get_param_by_name(binary(), [#just_sms_request_param_dto{}], term()) -> term().
get_param_by_name(Name, Params, Default) ->
	case lists:keyfind(Name, #just_sms_request_param_dto.name, Params) of
		false ->
			Default;
		#just_sms_request_param_dto{value = {_, Value}} ->
			Value
	end.

process_k1api_req(#just_sms_request_dto{
	client_type = k1api,
	id = RequestId,
	customer_id = CustomerId,
	user_id = _UserId,
	params = Params,
	source_addr = SourceAddr
}, SmsIds) ->
	InputMessageIds = lists:map(fun({_Addr, Id}) -> {CustomerId, k1api, Id} end, SmsIds),
	NotifyURL = get_param_by_name(<<"k1api_notify_url">>, Params, undefined),
	?log_debug("NotifyURL: ~p", [NotifyURL]),
	CallbackData = get_param_by_name(<<"k1api_callback_data">>, Params, undefined),
	?log_debug("CallbackData: ~p", [CallbackData]),
	SubId = create_k1api_receipt_subscription(CustomerId, <<"undefined">>, SourceAddr, NotifyURL, CallbackData),
	ok = link_input_id_to_sub_id(InputMessageIds, SubId),
	ok = link_sms_request_id_to_message_ids(CustomerId, undefined, SourceAddr, RequestId, InputMessageIds);
process_k1api_req(_, _) ->
	ok.

create_k1api_receipt_subscription(_, _, _, undefined, _) -> undefined;
create_k1api_receipt_subscription(CustomerId, UserId, DestAddr, NotifyURL, CallbackData) ->
	QName = <<"pmm.k1api.incoming">>,
	SubscriptionId = uuid:unparse(uuid:generate_time()),
	Subscription = #k_mb_k1api_receipt_sub{
		id = SubscriptionId,
		customer_id = CustomerId,
		user_id = UserId,
		queue_name = QName,
		dest_addr = DestAddr,
		notify_url = NotifyURL,
		callback_data = CallbackData,
		created_at = k_datetime:utc_timestamp()
	},
	ok = k_mailbox:register_sms_req_receipts_subscription(Subscription),
	SubscriptionId.

link_input_id_to_sub_id(_, undefined) -> ok;
link_input_id_to_sub_id([], _SubId) -> ok;
link_input_id_to_sub_id([InputId | RestIds], SubId) ->
	ok = k_mb_db:link_input_id_to_sub_id(InputId, SubId),
	link_input_id_to_sub_id(RestIds, SubId).

link_sms_request_id_to_message_ids(
	CustomerId, UserId, SenderAddress, SmsRequestId, InputMessageIds
) ->
	%% Include CustomerId & UserId to Key to avoid access to another's
	%% sms statuses
	%% Include SenderAddress into Key to make SmsRequestId unique
	%% within specific SenderAddress
	ok = k_k1api:link_sms_request_id_to_msg_ids(CustomerId, UserId, SenderAddress, SmsRequestId, InputMessageIds).
