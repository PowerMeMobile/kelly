-module(k_sms_request_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").

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
	Time = k_storage_util:utc_unix_epoch(),
	lists:foreach(fun(MsgInfo = #msg_info{
						id = Id,
						customer_id = CustomerId,
						registered_delivery = RegDelivery
					 }) ->
						InputId = {CustomerId, Id},
						ok = store_msg_info(InputId, MsgInfo, Time),
						ok = update_msg_status(InputId, submitted, RegDelivery, Time),
						?log_debug("Message stored and its status updated: ~p", [MsgInfo])
				  end, MsgInfos),
	{ok, []}.

-spec store_msg_info(msg_id(), #msg_info{}, integer()) -> ok.
store_msg_info(InputId, MsgInfo, Time) ->
	ok = k_storage_api:set_msg_info(InputId, MsgInfo),
	ok = k_reports_api:store_msg_stats(InputId, MsgInfo, Time).

-spec update_msg_status(msg_id(), atom(), boolean(), integer()) -> ok.
update_msg_status(InputId, DefaultStatus, RegDelivery, ReqTime) ->
	case k_storage_api:get_msg_status(InputId) of
		%% normal case, no response yet received.
		{error, no_entry} ->
			NewMsgStatus = #msg_status{
				status = DefaultStatus,
				req_time = ReqTime
			},
			ok = k_storage_api:set_msg_status(InputId, NewMsgStatus);
		%% response already received...
		{ok, #msg_status{status = Status} = MsgStatus} ->
				case Status of
					%% with failed status, update the request time and leave the status as it is.
					failure ->
						NewMsgStatus = MsgStatus#msg_status{req_time = ReqTime},
						ok = k_storage_api:set_msg_status(InputId, NewMsgStatus);
					%% with successful status, update the request time and the status.
					success ->
						NewStatus = case RegDelivery of
										true -> success_waiting_delivery;
										false -> success_no_delivery
									end,
						NewMsgStatus = MsgStatus#msg_status{status = NewStatus, req_time = ReqTime},
						ok = k_storage_api:set_msg_status(InputId, NewMsgStatus);
					%% unexpected message status.
					%% the most probably the same message was received again.
					Unexpected ->
						?log_error("Unexpected message status: ~p", [Unexpected])
				end
	end.

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
	customerId = CustomerId,
	type = Type,
	message = Message,
	encoding = Encoding,
	params = Params,
	sourceAddr = SourceAddr,
	destAddrs = {_, DestAddrs},
	messageIds = MessageIds
}) ->
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
					customer_id = CustomerId,
					type = Type,
					encoding = Encoding,
					body = list_to_binary(Message),
					source_addr = SourceAddr,
					dest_addr = DestAddr,
					registered_delivery = RegisteredDelivery
				} end, lists:zip(DestAddrs, MessageIds)).
