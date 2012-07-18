-module(k_sms_request_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/JustAsn.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	% ?log_debug("Got message: ~p", [Message]),
	case 'JustAsn':decode('SmsRequest', Message) of
		{ok, SmsRequest} ->
			process_sms_request(SmsRequest);
		Error ->
			Error
	end.

-spec process_sms_request(#'SmsRequest'{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_request(SmsRequest = #'SmsRequest'{}) ->
	?log_debug("Got request: ~p", [SmsRequest]),
	MsgInfos = sms_request_to_msg_info_list(SmsRequest),
	%?log_debug("~p", [MsgInfos]),
	case k_utils:safe_foreach(fun process_msg_info/1, MsgInfos, ok, {error, '_'}) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec process_msg_info(#msg_info{}) -> ok | {error, any()}.
process_msg_info(MsgInfo = #msg_info{
	id = Id,
	customer_id = CustomerId
}) ->
	InputId = {CustomerId, Id},
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
	ok = k_statistic:store_msg_stats(InputId, MsgInfo, Time).

-spec get_param_by_name(string(), [#'Param'{}]) -> {ok, #'Param'{}} | {error, no_entry}.
get_param_by_name(Name, Params) ->
	Result = lists:keyfind(Name, #'Param'.name, Params),
	case Result of
		false ->
			{error, no_entry};
		Param ->
			{ok, Param}
	end.

-spec sms_request_to_msg_info_list(#'SmsRequest'{}) -> [#msg_info{}].
sms_request_to_msg_info_list(#'SmsRequest'{
	id = _Id,
	gatewayId = GatewayId,
	customerId = CustomerId,
	type = Type,
	message = Message,
	encoding = Encoding,
	params = Params,
	sourceAddr = SourceAddr,
	destAddrs = {_, DestAddrs},
	messageIds = MessageIds
}) ->
	%% Message ids come in ["ID1", "ID2:ID3", "ID4"], where "ID2:ID3" is a multipart message ids.
	%% Destination addrs come in ["ADDR1", "ADDR2", "ADDR3"]. The task is to get {ADDRX, IDY} pairs
	%% like that [{"ADDR1", "ID1"}, {"ADDR2", "ID2"}, {"ADDR2", "ID3"}, {"ADDR3", "ID4"}].
	AllPairs = lists:foldr(
		fun({Addr, ID}, Acc) ->
			Ids = lists:map(fun(Id) -> {Addr, Id} end, explode($:, ID)),
			Ids ++ Acc
		end, [], lists:zip(DestAddrs, MessageIds)),
	RegisteredDelivery =
		case get_param_by_name("registered_delivery", Params) of
			{ok, #'Param'{value = {boolean, Value}}} ->
				Value;
			_ ->
				false
		end,
	lists:map(fun({DestAddr, MessageId}) ->
				#msg_info{
					id = MessageId,
					gateway_id = GatewayId,
					customer_id = CustomerId,
					type = Type,
					encoding = Encoding,
					body = list_to_binary(Message),
					src_addr = transform_addr(SourceAddr),
					dst_addr = transform_addr(DestAddr),
					registered_delivery = RegisteredDelivery
				} end, AllPairs).

transform_addr(#'FullAddr'{
	addr = Addr,
	ton = Ton,
	npi = Npi
}) ->
	#full_addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	};
transform_addr(#'FullAddrAndRefNum'{
	fullAddr = FullAddr,
	refNum = RefNum
}) ->
	#full_addr_ref_num{
		full_addr = transform_addr(FullAddr),
		ref_num = RefNum
	}.

%% it turned out to be the reimplementation of string:tokens/2 :)
-spec explode(Delimiter::char(), String::string()) -> [string()].
explode(Delimiter, String) ->
	explode(Delimiter, lists:reverse(String), [[]]).
explode(_, [], Groups) ->
	Groups;
explode(Delimiter, [Delimiter | String], Groups) ->
	explode(Delimiter, String, [[] | Groups]);
explode(Delimiter, [Char | String], [Group | Groups]) ->
	explode(Delimiter, String, [[Char | Group] | Groups]).
