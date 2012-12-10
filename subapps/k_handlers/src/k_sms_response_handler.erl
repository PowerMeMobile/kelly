-module(k_sms_response_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").

-record(msg_resp, {
	input_id :: msg_id(),
	output_id :: msg_id(),
	status :: atom()
}).

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#just_sms_response_dto{}, Message) of
		{ok, SmsResponse} ->
			process_sms_response(SmsResponse);
		Error ->
			Error
	end.

-spec process_sms_response(#just_sms_response_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_response(SmsResponse = #just_sms_response_dto{}) ->
	?log_debug("Got just sms response: ~p", [SmsResponse]),
	MsgResps = sms_response_to_msg_resp_list(SmsResponse),
	case k_utils:safe_foreach(fun process_msg_resp/1, MsgResps, ok, {error, '_'}) of
		ok ->
			{ok, []};
		%% abnormal case, sms request isn't handled yet.
		{error, no_entry} ->
			{error, not_enough_data_to_proceed};
		Error ->
			Error
	end.

-spec process_msg_resp(#msg_resp{}) -> ok | {error, any()}.
process_msg_resp(#msg_resp{
	input_id = InputId,
	output_id = OutputId,
	status = Status
}) ->
	%% check if the message is already registered, it's quite possible that it isn't.
	case k_storage:get_msg_status(InputId) of
		%% normal case, sms request is handled.
		{ok, MsgStatus} ->
			case update_msg_status(InputId, OutputId, MsgStatus, Status) of
				ok ->
					case map_in_to_out(InputId, OutputId) of
						ok ->
							?log_debug("ids mapped and status updated: in:~p out:~p st:~p", [InputId, OutputId, Status]);
						Error ->
							Error
					end;
				Error ->
					Error
			end;
		Error ->
			Error
	end.

-spec update_msg_status(msg_id(), msg_id(), #msg_status{}, atom()) -> ok | {error, any()}.
update_msg_status(InputId, OutputId, MsgStatus, ResponseStatus) ->
	RespTime = k_datetime:utc_unix_epoch(),
	case k_storage:get_msg_info(InputId) of
		{ok, MsgInfo} ->
			NewStatus = fix_status(ResponseStatus, MsgInfo#msg_info.reg_dlr),
			NewMsgStatus = MsgStatus#msg_status{
							 status = NewStatus,
							 resp_time = RespTime
							},
			%% update message status and stats.
			ok = k_storage:set_msg_status(InputId, NewMsgStatus),
			ok = k_statistic:store_status_stats(InputId, OutputId, MsgInfo, NewMsgStatus, RespTime);
		Error ->
			Error
	end.

fix_status(success, true) -> success_waiting_delivery;
fix_status(success, false) -> success_no_delivery;
fix_status(failure, _) -> failure.

-spec map_in_to_out(msg_id(), msg_id()) -> ok.
map_in_to_out(InputId, OutputId) ->
	ok = k_storage:map_input_id_to_output_id(InputId, OutputId),
	ok = k_storage:map_output_id_to_input_id(OutputId, InputId).

-spec sms_response_to_msg_resp_list(#just_sms_response_dto{}) -> [#msg_resp{}].
sms_response_to_msg_resp_list(#just_sms_response_dto{
	id = _Id,
	customer_id = CustomerId,
	client_type = ClientType,
	gateway_id = GatewayId,
	timestamp = _Timestamp,
	statuses = Statuses
}) ->
	lists:map(fun(#just_sms_status_dto{
					original_id = OriginalId,
					dest_addr = _DestAddr,
					status = Status,
					parts_total = _PartsTotal,
					part_index = _PartIndex,
					message_id = MessageId,
					error_code = _ErrorCode
				 }) ->
					#msg_resp{
						input_id = {CustomerId, ClientType, OriginalId},
						output_id = {GatewayId, MessageId},
						status = Status
					} end, Statuses).
