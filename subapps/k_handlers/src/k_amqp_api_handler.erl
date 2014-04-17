-module(k_amqp_api_handler).

-export([start_link/0]).
-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
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
process(ReqCT, ReqBin) when ReqCT =:= <<"CoverageReq">> ->
    case adto:decode(#k1api_coverage_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got coverage request: ~p", [ReqDTO]),
            case k_coverage_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built coverage response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"CoverageResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Coverage response decode error: ~p", [Error]),
                            {ReqCT, <<>>}
                    end;
                {error, Error} ->
                    ?log_error("Coverage request process error: ~p", [Error]),
                    {ReqCT, <<>>}
            end;
        {error, Error} ->
            ?log_error("Coverage request decode error: ~p", [Error]),
            {ReqCT, <<>>}
    end;
process(ReqCT, ReqBin) when ReqCT =:= <<"DeliveryStatusReq">> ->
    case adto:decode(#k1api_sms_delivery_status_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got delivery status request: ~p", [ReqDTO]),
            case k_delivery_status_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built delivery status response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"DeliveryStatusResp">>, RespBin};
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
process(ReqCT, ReqBin) when ReqCT =:= <<"RetrieveSmsReq">> ->
    case adto:decode(#k1api_retrieve_sms_request_dto{}, ReqBin) of
        {ok, ReqDTO} ->
            ?log_debug("Got retrieve sms request: ~p", [ReqDTO]),
            case k_retrieve_sms_request_processor:process(ReqDTO) of
                {ok, RespDTO} ->
                    ?log_debug("Built retrieve sms response: ~p", [RespDTO]),
                    case adto:encode(RespDTO) of
                        {ok, RespBin} ->
                            {<<"RetrieveSmsResp">>, RespBin};
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
process(ReqCT, ReqBin) ->
    ?log_error("Got unknown api request: ~p ~p", [ReqCT, ReqBin]),
    {ReqCT, <<>>}.
