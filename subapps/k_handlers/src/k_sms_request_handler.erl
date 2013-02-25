-module(k_sms_request_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
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

-spec get_param_by_name(string(), [#just_sms_request_param_dto{}]) -> {ok, #just_sms_request_param_dto{}} | {error, no_entry}.
get_param_by_name(Name, Params) ->
	Result = lists:keyfind(Name, #just_sms_request_param_dto.name, Params),
	case Result of
		false ->
			undefined;
		#just_sms_request_param_dto{value = {_, Value}} ->
			Value
	end.

-spec sms_request_to_req_info_list(#just_sms_request_dto{}) -> [#req_info{}].
sms_request_to_req_info_list(SmsRequest = #just_sms_request_dto{
	id = _SmsRequestID,
	gateway_id = GatewayId,
	customer_id = CustomerId,
	client_type = ClientType,
	type = Type,
	message = Message,
	encoding = Encoding,
	params = Params,
	source_addr = SourceAddr,
	dest_addrs = {_, DestAddrs},
	message_ids = MessageIds}) ->
	%% Message ids come in ["ID1", "ID2:ID3", "ID4"], where "ID2:ID3" is a multipart message ids.
	%% Destination addrs come in ["ADDR1", "ADDR2", "ADDR3"]. The task is to get {ADDRX, IDY} pairs
	%% like that [{"ADDR1", "ID1"}, {"ADDR2", "ID2"}, {"ADDR2", "ID3"}, {"ADDR3", "ID4"}].
	AllPairs = lists:foldr(
		fun({Addr, ID}, Acc) ->
			Ids = lists:map(fun(Id) -> {Addr, Id} end, split(ID)),
			Ids ++ Acc
		end, [], lists:zip(DestAddrs, MessageIds)),
	RegDlr =
		case get_param_by_name(<<"registered_delivery">>, Params) of
			undefined -> false;
			Any -> Any
		end,
	process_k1api_req(SmsRequest, AllPairs),
	lists:map(fun({DestAddr, MessageId}) ->
				#req_info{
					client_type = ClientType,
					customer_id = CustomerId,
					in_msg_id = MessageId,
					gateway_id = GatewayId,
					type = Type,
					encoding = Encoding,
					body = Message,
					src_addr = SourceAddr,
					dst_addr = DestAddr,
					reg_dlr = RegDlr,
					req_time = k_datetime:utc_timestamp()
				} end, AllPairs).

-spec split(binary()) -> [binary()].
split(BinIDs) ->
	binary:split(BinIDs, <<":">>, [global, trim]).

process_k1api_req(SmsRequest = #just_sms_request_dto{client_type = k1api}, SmsIDs) ->
	#just_sms_request_dto{
		id = SmsRequestID,
		customer_id = CustomerID,
		params = Params,
		source_addr = SourceAddr
	} = SmsRequest,
	InputMessageIDs = lists:map(fun({_Addr, ID}) ->
		{CustomerID, k1api, ID}
	end, SmsIDs),
	NotifyURL = get_param_by_name(<<"k1api_notify_url">>, Params),
	?log_debug("NotifyURL: ~p", [NotifyURL]),
	CallbackData = get_param_by_name(<<"k1api_callback_data">>, Params),
	?log_debug("CallbackData: ~p", [CallbackData]),
	SubID = create_k1api_receipt_subscription(CustomerID, <<"undefined">>, SourceAddr, NotifyURL, CallbackData),
	ok = link_input_id_to_sub_id(InputMessageIDs, SubID),
	ok = link_sms_request_id_to_message_ids(CustomerID, undefined, SourceAddr, SmsRequestID, InputMessageIDs);
process_k1api_req(_, _) ->
	ok.

create_k1api_receipt_subscription(_, _, _, undefined, _) -> undefined;
create_k1api_receipt_subscription(CustomerID, UserID, DestAddr, NotifyURL, CallbackData) ->
	QName = <<"pmm.k1api.incoming">>,
	SubscriptionID = uuid:newid(),
	Subscription = #k_mb_k1api_receipt_sub{
		id = SubscriptionID,
		customer_id = CustomerID,
		user_id = UserID,
		queue_name = QName,
		dest_addr = DestAddr,
		notify_url = NotifyURL,
		callback_data = CallbackData,
		created_at = k_datetime:utc_timestamp()
	},
	ok = k_mailbox:register_sms_req_receipts_subscription(Subscription),
	SubscriptionID.

link_input_id_to_sub_id(_, undefined) -> ok;
link_input_id_to_sub_id([], _SubID) -> ok;
link_input_id_to_sub_id([InputID | RestIDs], SubID) ->
	ok = k_mb_db:link_input_id_to_sub_id(InputID, SubID),
	link_input_id_to_sub_id(RestIDs, SubID).

link_sms_request_id_to_message_ids(
	CustomerID, UserID, SenderAddress, SmsRequestID, InputMessageIDs
) ->
	%% Include CustomerID & UserID to Key to avoid access to another's
	%% sms statuses
	%% Include SenderAddress into Key to make SmsRequestID unique
	%% within specific SenderAddress

	ok = k_k1api:link_sms_request_id_to_msg_ids(CustomerID, UserID, SenderAddress, SmsRequestID, InputMessageIDs).
