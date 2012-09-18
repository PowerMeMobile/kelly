-module(k_sms_request_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(ContentType, Message) ->
	case adto:decode(#just_sms_request_dto{}, Message) of
		{ok, SmsRequest} ->
			content_type_to_client_type(SmsRequest, ContentType);
		Error ->
			Error
	end.

content_type_to_client_type(SmsRequest, <<"k1apiSmsRequest">>) ->
	process_sms_request(SmsRequest, k1api);
content_type_to_client_type(SmsRequest, <<"SmsRequest">>) ->
	process_sms_request(SmsRequest, funnel).

-spec process_sms_request(#just_sms_request_dto{}, k1api | funnel) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_request(SmsRequest, ClientType) ->
	?log_debug("Got ~p sms request: ~p", [ClientType, SmsRequest]),
	MsgInfos = sms_request_to_msg_info_list(SmsRequest, ClientType),
	case k_utils:safe_foreach(fun process_msg_info/1, MsgInfos, ok, {error, '_'}) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec process_msg_info(#msg_info{}) -> ok | {error, any()}.
process_msg_info(MsgInfo = #msg_info{
	id = Id,
	client_type = ClientType,
	customer_id = CustomerId
}) ->
	InputId = {CustomerId, ClientType, Id},
	Status = submitted,
	Time = k_datetime:utc_unix_epoch(),
	case update_msg_status(InputId, Status, Time) of
		ok ->
			case store_msg_info(InputId, MsgInfo, Time) of
				ok ->
					?log_debug("Message stored and its status updated: in:~p st:~p", [InputId, Status]);
				Error ->
					Error
			end;
		Error ->
			Error
	end.

-spec update_msg_status(msg_id(), atom(), integer()) -> ok | {error, any()}.
update_msg_status(InputId, DefaultStatus, ReqTime) ->
	case k_storage:get_msg_status(InputId) of
		%% normal case, no data stored yet.
		{error, no_entry} ->
			NewMsgStatus = #msg_status{
				status = DefaultStatus,
				req_time = ReqTime
			},
			ok = k_storage:set_msg_status(InputId, NewMsgStatus);
		%% Very strange case. It shouldn't be here, but I saw it happened during the testing.
		%% I'm not sure what to do in this case. :(
		{ok, _MsgStatus} ->
			?log_info("~p", [_MsgStatus]),
			ok;
		Other ->
			Other
	end.

-spec store_msg_info(msg_id(), #msg_info{}, integer()) -> ok.
store_msg_info(InputId, MsgInfo, Time) ->
	ok = k_storage:set_msg_info(InputId, MsgInfo),
	ok = k_statistic:store_outgoing_msg_stats(InputId, MsgInfo, Time).

-spec get_param_by_name(string(), [#just_sms_request_param_dto{}]) -> {ok, #just_sms_request_param_dto{}} | {error, no_entry}.
get_param_by_name(Name, Params) ->
	Result = lists:keyfind(Name, #just_sms_request_param_dto.name, Params),
	case Result of
		false ->
			{error, no_entry};
		Param ->
			{ok, Param}
	end.

-spec sms_request_to_msg_info_list(#just_sms_request_dto{}, k1api | funnel) -> [#msg_info{}].
sms_request_to_msg_info_list(#just_sms_request_dto{
	id = _Id,
	gateway_id = GatewayId,
	customer_id = CustomerId,
	type = Type,
	message = Message,
	encoding = Encoding,
	params = Params,
	source_addr = SourceAddr,
	dest_addrs = {_, DestAddrs},
	message_ids = MessageIds}, ClientType) ->
	%% Message ids come in ["ID1", "ID2:ID3", "ID4"], where "ID2:ID3" is a multipart message ids.
	%% Destination addrs come in ["ADDR1", "ADDR2", "ADDR3"]. The task is to get {ADDRX, IDY} pairs
	%% like that [{"ADDR1", "ID1"}, {"ADDR2", "ID2"}, {"ADDR2", "ID3"}, {"ADDR3", "ID4"}].
	AllPairs = lists:foldr(
		fun({Addr, ID}, Acc) ->
			Ids = lists:map(fun(Id) -> {Addr, Id} end, split(ID)),
			Ids ++ Acc
		end, [], lists:zip(DestAddrs, MessageIds)),
	RegisteredDelivery =
		case get_param_by_name("registered_delivery", Params) of
			{ok, #just_sms_request_param_dto{value = {boolean, Value}}} ->
				Value;
			_ ->
				false
		end,
	lists:map(fun({DestAddr, MessageId}) ->
				#msg_info{
					id = MessageId,
					gateway_id = GatewayId,
					customer_id = CustomerId,
					client_type = ClientType,
					type = Type,
					encoding = Encoding,
					body = Message,
					src_addr = transform_addr(SourceAddr),
					dst_addr = transform_addr(DestAddr),
					registered_delivery = RegisteredDelivery
				} end, AllPairs).

transform_addr(#addr_dto{
	addr = Addr,
	ton = Ton,
	npi = Npi
}) ->
	#full_addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	};
transform_addr(#addr_ref_num_dto{
	full_addr = FullAddr,
	ref_num = RefNum
}) ->
	#full_addr_ref_num{
		full_addr = transform_addr(FullAddr),
		ref_num = RefNum
	}.


-spec split(binary()) -> [binary()].
split(BinIDs) ->
	binary:split(BinIDs, <<":">>, [global, trim]).
