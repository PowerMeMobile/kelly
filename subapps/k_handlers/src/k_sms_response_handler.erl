-module(k_sms_response_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
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
	RespInfos = sms_response_to_resp_info_list(SmsResponse),
	case k_utils:safe_foreach(
		fun k_dynamic_storage:set_mt_resp_info/1, RespInfos, ok, {error, '_'}
	) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec sms_response_to_resp_info_list(#just_sms_response_dto{}) -> [#resp_info{}].
sms_response_to_resp_info_list(#just_sms_response_dto{
	id = RequestId,
	customer_id = CustomerId,
	client_type = ClientType,
	gateway_id = GatewayId,
	timestamp = UTCString,
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
					#resp_info{
						req_id = RequestId,
						client_type = ClientType,
						customer_id = CustomerId,
						in_msg_id = OriginalId,
						gateway_id = GatewayId,
						out_msg_id = MessageId,
						resp_time = k_datetime:utc_string_to_timestamp(UTCString),
						resp_status = Status
					} end, Statuses).
