-module(k_http_api_handler_message_status).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include("gen_cowboy_restful_spec.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_status.hrl").

-record(state, {
	mesid :: string(),
	custid :: string()
}).

%%% REST parameters
-record(get, {
}).

init(_Req, 'GET', [<<"message_status">>, MesBinId, <<"customer">>, CustUUIDBin]) ->
	MesId = binary_to_list(MesBinId),
	CustUUID = binary_to_list(CustUUIDBin),
	{ok, #get{}, #state{mesid = MesId, custid = CustUUID}};

init(_Req, HttpMethod, Path) ->
	?log_debug("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{}, State = #state{mesid = MesId, custid = CustUUID}) ->
	Response =
		case k_storage:get_msg_status({CustUUID, MesId}) of
			{ok, #msg_status{status = Status}} ->
				{message, [{customer_id, CustUUID}, {message_id, MesId}, {status, Status}]};
			Any ->
				Any
		end,
	{ok, Response, State}.

terminate(_Req, _State = #state{}) ->
    ok.
