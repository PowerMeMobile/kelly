-module(k_amqp_api_handler).

-export([start_link/0]).
-export([process/2]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/network.hrl").
-include_lib("k_storage/include/network_map.hrl").

-type req_ct()  :: binary().
-type req_bin() :: binary().
-type resp_ct() :: binary().
-type resp_bin() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Queue} = application:get_env(k_handlers, kelly_api_queue),
    rmql_rpc_server:start_link(?MODULE, Queue, fun ?MODULE:process/2).

%% ===================================================================
%% Internal
%% ===================================================================

-spec process(req_ct(), req_bin()) -> {resp_ct(), resp_bin()} | {req_ct(), <<>>}.
process(ReqCT, ReqBin) when ReqCT =:= <<"CoverageReqV1">> ->
    case adto:decode(#coverage_req_v1{}, ReqBin) of
        {ok, #coverage_req_v1{req_id = ReqId} = Req} ->
            ?log_debug("Got coverage request: ~p", [Req]),
            case k_coverage_request_processor:process(Req) of
                {ok, #coverage_resp_v1{} = Resp} ->
                    ?log_debug("Built coverage response: ~p", [Resp]),
                    {ok, RespBin} = adto:encode(Resp),
                    {<<"CoverageRespV1">>, RespBin};
                {error, Error} ->
                    ?log_error("Coverage request process error: ~p", [Error]),
                    ErrResp = #error_resp_v1{
                        req_id = ReqId,
                        error = Error
                    },
                    {ok, ErrRespBin} = adto:encode(ErrResp),
                    {<<"ErrorRespV1">>, ErrRespBin}
            end;
        {error, Error} ->
            ?log_error("Coverage request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"BlacklistReqV1">> ->
    case adto:decode(#blacklist_req_v1{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got blacklist request: ~p", [ReqDTO]),
            case k_blacklist_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built blacklist response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"BlacklistRespV1">>, RespBin};
                        {error, Error} ->
                            ?log_error("Blacklist response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Blacklist request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Blacklist request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"SmsStatusReqV1">> ->
    case adto:decode(#sms_status_req_v1{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got delivery status request: ~p", [ReqDTO]),
            case k_delivery_status_request_processor:process(ReqDTO) of
                {ok, #sms_status_resp_v1{} = RespDTO} ->
                    ?log_debug("Built delivery status response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"SmsStatusRespV1">>, RespBin};
                        {error, Error} ->
                            ?log_error("Delivery status response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Delivery status request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Delivery status request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"RetrieveSmsReqV1">> ->
    case adto:decode(#retrieve_sms_req_v1{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got retrieve sms request: ~p", [ReqDTO]),
            case k_retrieve_sms_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built retrieve sms response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"RetrieveSmsRespV1">>, RespBin};
                        {error, Error} ->
                            ?log_error("Retrieve sms response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Retrieve sms request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Retrieve sms request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"CreditReqV1">> ->
    case adto:decode(#credit_req_v1{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got request credit request: ~p", [ReqDTO]),
            case k_request_credit_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built request credit response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"CreditRespV1">>, RespBin};
                        {error, Error} ->
                            ?log_error("request credit response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("request credit request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("request credit request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"SubscribeSmsReceiptsReq">> ->
    case adto:decode(#k1api_subscribe_sms_receipts_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got subscribe sms receipts request: ~p", [ReqDTO]),
            case k_subscribe_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built subscribe sms receipts response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"SubscribeSmsReceiptsResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Subscribe sms receipts response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Subscribe sms receipts request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Subscribe sms receipts request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"UnsubscribeSmsReceiptsReq">> ->
    case adto:decode(#k1api_unsubscribe_sms_receipts_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got unsubscribe sms receipts request: ~p", [ReqDTO]),
            case k_subscribe_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built unsubscribe sms receipts response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"UnsubscribeSmsReceiptsResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Unsubscribe sms receipts response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Unsubscribe sms receipts request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Unsubscribe sms receipts request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"SubscribeIncomingSmsReq">> ->
    case adto:decode(#k1api_subscribe_incoming_sms_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got subscribe incoming sms request: ~p", [ReqDTO]),
            case k_subscribe_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built subscribe incoming sms response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"SubscribeIncomingSmsResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Subscribe incoming sms response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Subscribe incoming sms request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Subscribe incoming sms request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"UnsubscribeIncomingSmsReq">> ->
    case adto:decode(#k1api_unsubscribe_incoming_sms_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got unsubscribe incoming sms request: ~p", [ReqDTO]),
            case k_subscribe_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built unsubscribe incoming sms response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"UnsubscribeIncomingSmsResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Unsubscribe incoming sms response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Unsubscribe incoming sms request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Unsubscribe incoming sms request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) when ReqCT =:= <<"InboxReqV1">> ->
    case adto:decode(#inbox_req_v1{}, ReqBin) of
        {ok, #inbox_req_v1{req_id = ReqId} = Req} ->
            ?log_debug("Got inbox request: ~p", [Req]),
            case k_inbox_processor:process(Req) of
                {ok, Resp} ->
                    ?log_debug("Built inbox response: ~p", [Resp]),
                    case adto:encode(Resp) of
                        {ok, RespBin} ->
                            {<<"InboxRespV1">>, RespBin};
                        {error, Error} ->
                            ?log_error("Inbox response decode error: ~p", [Error]),
                            ErrResp = #error_resp_v1{
                                req_id = ReqId,
                                error = Error
                            },
                            {ok, ErrRespBin} = adto:encode(ErrResp),
                            {<<"ErrorRespV1">>, ErrRespBin}
                    end;
                {error, Error} ->
                    ?log_error("Inbox request process error: ~p", [Error]),
                    ErrResp = #error_resp_v1{
                        req_id = ReqId,
                        error = Error
                    },
                    {ok, ErrRespBin} = adto:encode(ErrResp),
                    {<<"ErrorRespV1">>, ErrRespBin}
            end;
        {error, Error} ->
            ?log_error("Inbox request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;

process(ReqCT, ReqBin) ->
    ?log_error("Got unknown api request: ~p ~p", [ReqCT, ReqBin]),
    {ReqCT, <<>>}.
