-module(k_sms_request_deferred_handler).

-export([process/1]).
-compile({no_auto_import, [split_binary/2]}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
   -compile(export_all).
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    try
        process(ContentType, Payload)
    catch
        Class:Error ->
            Stacktrace = erlang:get_stacktrace(),
            ?log_error("Exception: ~p:~p Stacktrace: ~p", [Class, Error, Stacktrace]),
            {ok, []}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

process(<<"SmsReqV1">>, ReqBin) ->
    {ok, SmsReq} = adto:decode(#sms_req_v1{}, ReqBin),
    process_sms_req(SmsReq);
process(<<"SmsReqV1z">>, ReqBinZ) ->
    ReqBin = zlib:uncompress(ReqBinZ),
    {ok, SmsReq} = adto:decode(#sms_req_v1{}, ReqBin),
    process_sms_req(SmsReq);
process(ReqCT, ReqBin) ->
    ?log_error("Got unknown sms request: ~p ~p", [ReqCT, ReqBin]),
    {ok, []}.

process_sms_req(#sms_req_v1{} = SmsReq) ->
    ?log_debug("Got deferred sms request: ~p", [SmsReq]),
    ReqTime = ac_datetime:utc_timestamp(),
    ReqInfos = k_sms_request_handler:v1_sms_req_to_req_infos(SmsReq, ReqTime),
    BatchInfo = k_sms_request_handler:v1_build_batch_info(SmsReq, ReqTime, ReqInfos),
    case k_storage_defers:set_mt_def_batch_info(BatchInfo, SmsReq) of
        ok ->
            {ok, []};
        Error ->
            Error
    end.
