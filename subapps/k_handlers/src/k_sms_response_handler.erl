-module(k_sms_response_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").

-record(msg_resp, {
	input_id :: msg_id(),
	output_id :: msg_id(),
	status :: atom()
}).

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	% ?log_debug("got message...", []),
	case 'JustAsn':decode('SmsResponse', Message) of
		{ok, SmsResponse} ->
			process_sms_response(SmsResponse);
		Error ->
			Error
	end.

-spec process_sms_response(#'SmsResponse'{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_response(SmsResponse = #'SmsResponse'{}) ->
	?log_debug("got request: ~p", [SmsResponse]),
	MsgResps = sms_response_to_msg_resp_list(SmsResponse),
	ok = store_gtw_stats(SmsResponse),
	lists:foreach(fun(MsgResp = #msg_resp{
						input_id = InputId,
						output_id = OutputId,
						status = Status
					  }) ->
						ok = map_in_to_out(InputId, OutputId),
						ok = update_msg_status(InputId, Status),
						ok = process_unhandled_dlr_status(InputId, OutputId),
						?log_debug("ids mapped and status updated: ~p", [MsgResp])
				  end, MsgResps),
	{ok, []}.

-spec map_in_to_out(msg_id(), msg_id()) -> ok.
map_in_to_out(InputId, OutputId) ->
	ok = k_storage_api:map_input_id_to_output_id(InputId, OutputId),
	ok = k_storage_api:map_output_id_to_input_id(OutputId, InputId).

-spec update_msg_status(msg_id(), atom()) -> ok.
update_msg_status(InputId, ResponseStatus) ->
	RespTime = k_storage_util:utc_unix_epoch(),
	%% check if the message is already registered, it's quite possible that it isn't.
	case k_storage_api:get_msg_status(InputId) of
		{ok, MsgStatus} ->
			case k_storage_api:get_msg_info(InputId) of
				{ok, #msg_info{registered_delivery = RegDelivery}} ->
					NewStatus = case ResponseStatus of
									success ->
										case RegDelivery of
											true -> success_waiting_delivery;
											false -> success_no_delivery
										end;
									failure ->
										failure
								end,
					NewMsgStatus = MsgStatus#msg_status{
						status = NewStatus,
						resp_time = RespTime
					},
					ok = k_storage_api:set_msg_status(InputId, NewMsgStatus);
				{error, no_entry} ->
					%% this means that the response was already received,
					%% but the message info wasn't yet stored.
					NewMsgStatus = MsgStatus#msg_status{
						status = ResponseStatus, % success | failure
						resp_time = RespTime
					},
					ok = k_storage_api:set_msg_status(InputId, NewMsgStatus)
			end;
		{error, no_entry} ->
			%% this means that the response came before the message was registered,
			%% store its response status and the response time.
			NewMsgStatus = #msg_status{
				status = ResponseStatus, % success | failure
				resp_time = RespTime
			},
			ok = k_storage_api:set_msg_status(InputId, NewMsgStatus)
	end.

-spec process_unhandled_dlr_status(msg_id(), msg_id()) -> ok.
process_unhandled_dlr_status(InputId, OutputId) ->
	case k_storage_api:get_dlr_msg_status(OutputId) of
		{ok, #msg_status{status = DlrStatus, dlr_time = DlrTime}} ->
			case k_storage_api:get_msg_status(InputId) of
				{ok, MsgStatus} ->
					case k_storage_api:get_msg_info(InputId) of
						{ok, #msg_info{source_addr = SrcAddr, dest_addr = DstAddr}} ->
							%% update delivery status.
							ok = k_storage_api:set_msg_status(InputId, MsgStatus#msg_status{status = DlrStatus, dlr_time = DlrTime}),
							%% ??? Roma, what's going on here? :)
							{CustomerId, InputMsgId} = InputId,
							Data = {CustomerId, InputMsgId, DlrStatus, SrcAddr, DstAddr, DlrTime},
							{CustomerId, BatchId, BatchBinary} = k_funnel_asn_helper:render_receipt(Data),
							UserID = undefined,
							ok = k_mailbox:register_incoming_item(BatchId, CustomerId, UserID, <<"ReceiptBatch">>, BatchBinary),
							%% delete handled status.
							ok = k_storage_api:delete_dlr_msg_status(OutputId);
						{error, Reason} ->
							?log_error("Impossible to get msg info with reason: ~p", [Reason])
					end;
				{error, Reason} ->
					?log_error("Impossible to get msg status with reason: ~p", [Reason])
			end;
		{error, no_entry} ->
			%% it's ok. nothing to process.
			ok
	end.

-spec sms_response_to_msg_resp_list(#'SmsResponse'{}) -> [#msg_resp{}].
sms_response_to_msg_resp_list(#'SmsResponse'{
	id = _Id,
	customerId = CustomerId,
	gatewayId = GatewayId,
	timestamp = _Timestamp,
	statuses = Statuses
}) ->
	lists:map(fun(#'SmStatus'{
					originalId = OriginalId,
					destAddr = _DestAddr,
					status = Status,
					partsTotal = _PartsTotal,
					partIndex = _PartIndex,
					messageId = MessageId,
					errorCode = _ErrorCode
				 }) ->
					#msg_resp{
						input_id = {CustomerId, OriginalId},
						output_id = {GatewayId, MessageId},
						status = Status
					} end, Statuses).

-spec store_gtw_stats(#'SmsResponse'{}) -> ok | {error, any()}.
store_gtw_stats(#'SmsResponse'{
	gatewayId = GatewayId,
	statuses = Statuses
}) ->
	Number = length(Statuses),
	Time = k_storage_util:utc_unix_epoch(),
	k_reports_api:store_gtw_stats(GatewayId, Number, Time).
