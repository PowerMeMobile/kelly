-module(k_sms_response_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% Keep in sync with just_worker.erl
-define(ERROR_TIMEOUT,  16#000000400).
-define(ERROR_CLOSED,   16#000000401).
-define(ERROR_EXPIRED,  16#000000402).
-define(ERROR_CUSTOMER, 16#000000403).
-define(ERROR_BLOCKED,  16#000000404).

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(ContentType, Payload).

%% ===================================================================
%% Internal
%% ===================================================================

process(<<"SmsResponse">>, ReqBin) ->
    case adto:decode(#just_sms_response_dto{}, ReqBin) of
        {ok, SmsResp} ->
            process_sms_response(SmsResp);
        Error ->
            Error
    end;
process(ReqCT, ReqBin) ->
    ?log_error("Got unknown sms response: ~p ~p", [ReqCT, ReqBin]),
    {ok, []}.

-spec process_sms_response(#just_sms_response_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_response(SmsResp = #just_sms_response_dto{}) ->
    ?log_debug("Got sms response: ~p", [SmsResp]),
    RespInfos = sms_response_to_resp_info_list(SmsResp),
    case ac_utils:safe_foreach(
        fun k_dynamic_storage:set_mt_resp_info/1, RespInfos, ok, {error, '_'}
    ) of
        ok ->
            case ac_utils:safe_foreach(
                fun check_failed_responses/1, RespInfos, ok, {error, '_'}
            ) of
                ok ->
                    {ok, []};
                {error, req_info_unavailable} ->
                    %% req isn't handled yet.
                    %% wait for a while, then requeue it.
                    {ok, Timeout} =
                        application:get_env(k_handlers, response_retry_timeout),
                    timer:sleep(Timeout),
                    {error, not_enough_data_to_proceed};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

check_failed_responses(RespInfo) when
        RespInfo#resp_info.resp_status =:= failed orelse
        RespInfo#resp_info.resp_status =:= blocked ->
    ReqId = RespInfo#resp_info.req_id,
    InMsgId = RespInfo#resp_info.in_msg_id,
    {ok, MsgInfo} = k_dynamic_storage:get_mt_msg_info(ReqId, InMsgId),
    case MsgInfo#msg_info.req_time of
        {0,0,0} ->
            %% response came before request. it happens sometimes.
            {error, req_info_unavailable};
        _ ->
            case MsgInfo#msg_info.reg_dlr of
                false ->
                    ok;
                true ->
                    RespTime = MsgInfo#msg_info.resp_time,
                    MsgInfo2 = MsgInfo#msg_info{dlr_time = RespTime, status = rejected},
                    k_receipt_batch_handler:register_delivery_receipt(MsgInfo2)
            end
    end;
check_failed_responses(_RespInfo) ->
    ok.

-spec sms_response_to_resp_info_list(#just_sms_response_dto{}) -> [#resp_info{}].
sms_response_to_resp_info_list(SmsResponse) ->
    Statuses = SmsResponse#just_sms_response_dto.statuses,
    [convert(SmsResponse, S) || S <- Statuses].

-spec convert(#just_sms_response_dto{}, #just_sms_status_dto{}) -> #resp_info{}.
convert(SmsResponse, SmsStatus) ->
    #just_sms_response_dto{
        id = RequestId,
        customer_id = CustomerUuid,
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

    {Status2, ErrorCode2} = fix_status(Status, ErrorCode),
    #resp_info{
        req_id = RequestId,
        customer_uuid = CustomerUuid,
        in_msg_id = OriginalId,
        gateway_id = GatewayId,
        out_msg_id = MessageId,
        resp_time = ac_datetime:utc_string_to_timestamp(UTCString),
        resp_status = Status2,
        resp_error_code = ErrorCode2
    }.

fix_status(success, undefined) ->
    {submitted, undefined};
fix_status(failure, ?ERROR_BLOCKED) ->
    {blocked, undefined};
fix_status(failure, ErrorCode) ->
    {failed, ErrorCode}.
