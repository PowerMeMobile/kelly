-module(k_sms_response_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").

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
	MsgInfos = sms_response_to_msg_info_list(SmsResponse),
	case k_utils:safe_foreach(fun process_msg_info/1, MsgInfos, ok, {error, '_'}) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec process_msg_info(#msg_info{}) -> ok | {error, any()}.
process_msg_info(MsgInfo = #msg_info{}) ->
	ok = k_storage:set_outgoing_msg_info(MsgInfo).

-spec sms_response_to_msg_info_list(#just_sms_response_dto{}) -> [#msg_info{}].
sms_response_to_msg_info_list(#just_sms_response_dto{
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
					#msg_info{
						client_type = ClientType,
						customer_id = CustomerId,
						in_msg_id = OriginalId,
						gateway_id = GatewayId,
						out_msg_id = MessageId,
						resp_time = k_datetime:utc_timestamp(),
						resp_status = Status
					} end, Statuses).
