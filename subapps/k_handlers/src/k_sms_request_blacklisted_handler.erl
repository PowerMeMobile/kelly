-module(k_sms_request_blacklisted_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload0} = k_amqp_req:payload(Req),
    Payload = zlib:uncompress(Payload0),
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

process(<<"BlacklistedV1z">>, ReqBin) ->
    {ok, SmsRequestBlacklisted} = adto:decode(#blacklisted_v1{}, ReqBin),
    #blacklisted_v1{
        req_id = ReqId,
        numbers = Numbers
    } = SmsRequestBlacklisted,
    ?log_debug("Got blacklisted sms request: ~p", [SmsRequestBlacklisted]),
    case k_storage_blacklisted_request:set_blacklisted(ReqId, Numbers) of
        ok ->
            {ok, []};
        Error ->
            Error
    end;

process(ReqCT, ReqBin) ->
    ?log_error("Got unknown request: ~p ~p", [ReqCT, ReqBin]),
    {ok, []}.
