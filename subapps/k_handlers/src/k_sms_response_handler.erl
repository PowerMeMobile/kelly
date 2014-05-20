-module(k_sms_response_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, Payload} = k_amqp_req:payload(Req),
    case adto:decode(#just_sms_response_dto{}, Payload) of
        {ok, SmsResponse} ->
            process_sms_response(SmsResponse);
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec process_sms_response(#just_sms_response_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_response(SmsResponse = #just_sms_response_dto{}) ->
    ?log_debug("Got sms response: ~p", [SmsResponse]),
    RespInfos = sms_response_to_resp_info_list(SmsResponse),
    case ac_utils:safe_foreach(
        fun k_dynamic_storage:set_mt_resp_info/1, RespInfos, ok, {error, '_'}
    ) of
        ok ->
            {ok, []};
        Error ->
            Error
    end.

-spec sms_response_to_resp_info_list(#just_sms_response_dto{}) -> [#resp_info{}].
sms_response_to_resp_info_list(SmsResponse) ->
    Statuses = SmsResponse#just_sms_response_dto.statuses,
    Fun = fun(S) -> convert(SmsResponse, S) end,
    [Fun(S) || S <- Statuses].

-spec convert(#just_sms_response_dto{}, #just_sms_status_dto{}) -> #resp_info{}.
convert(SmsResponse, SmsStatus) ->
    #just_sms_response_dto{
        id = RequestId,
        customer_id = CustomerId,
        client_type = ClientType,
        gateway_id = GatewayId,
        timestamp = UTCString
    } = SmsResponse,

    #just_sms_status_dto{
        original_id = OriginalId,
        dest_addr = _DestAddr,
        status = Status,
        parts_total = _PartsTotal,
        part_index = _PartIndex,
        message_id = MessageId,
        error_code = ErrorCode
    } = SmsStatus,

    #resp_info{
        req_id = RequestId,
        customer_id = CustomerId,
        client_type = ClientType,
        in_msg_id = OriginalId,
        gateway_id = GatewayId,
        out_msg_id = MessageId,
        resp_time = ac_datetime:utc_string_to_timestamp(UTCString),
        resp_status = fix_status(Status),
        resp_error_code = ErrorCode
    }.

fix_status(success) -> sent;
fix_status(failure) -> failed;
fix_status(Status)  -> Status.
