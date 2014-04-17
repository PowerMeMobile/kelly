-module(k_amqp_api_handler).

-export([start_link/0]).
-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/network.hrl").
-include_lib("k_storage/include/network_map.hrl").

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

-spec process(binary(), binary()) -> {binary(), binary()} | {ok, []}.
process(<<"CoverageReq">>, ReqBin) ->
    case adto:decode(#k1api_coverage_request_dto{}, ReqBin) of
        {ok, CoverageReqDTO} ->
            ?log_debug("Got coverage request: ~p", [CoverageReqDTO]),
            case k_coverage_request_processor:process(CoverageReqDTO) of
                {ok, CoverageRespDTO} ->
                    ?log_debug("Built coverage response: ~p", [CoverageRespDTO]),
                    case adto:encode(CoverageRespDTO) of
                        {ok, RespBin} ->
                            {<<"CoverageResp">>, RespBin};
                        {error, Error} ->
                            ?log_error("Coverage response decode error: ~p", [Error]),
                            {ok, []}
                    end;
                {error, Error} ->
                    ?log_error("Coverage request process error: ~p", [Error]),
                    {ok, []}
            end;
        {error, Error} ->
            ?log_error("Coverage requeste decode error: ~p", [Error]),
            {ok, []}
    end.
