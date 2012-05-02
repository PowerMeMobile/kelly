-module(k_http_api_handler_users).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	cstid :: string(),
	usrid :: string()
}).

%%% REST parameters

-record(create, {
	id 						= {mandatory, <<"id">>, list},
	pswd 					= {mandatory, <<"pswd">>, list},
	permitted_smpp_types 	= {mandatory, <<"permitted_smpp_types">>, list} %% "transmitter,receiver,transceiver"
}).

-record(update, {
}).

-record(delete, {
}).

init(_Req, 'POST', [<<"customer">>, CstIdBin, <<"user">>]) ->
	CstId = binary_to_list(CstIdBin),
	{ok, #create{}, #state{cstid = CstId}};

init(_Req, 'PUT', [<<"customer">>, _CstIdBin, <<"user">>, _UserId]) ->
	{ok, #update{}, #state{}};

init(_Req, 'DELETE', [<<"customer">>, CstIdBin, <<"user">>, UserIdBin]) ->
	CstId = binary_to_list(CstIdBin),
	UserId = binary_to_list(UserIdBin),
	{ok, #delete{}, #state{cstid = CstId, usrid = UserId}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.


handle(_Req, #create{
				id 						= UserId,
				pswd 	 				= Pass,
				permitted_smpp_types 	= TypesString
					}, State = #state{cstid = CstId}) ->

	User = #user{
		id 						= UserId,
		pswd_hash 				= crypto:sha(Pass),
		permitted_smpp_types 	= convert(TypesString)
	},

	Res = k_aaa_api:set_customer_user(User, CstId),
	{ok, {result, Res}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{}, State = #state{cstid = CstId, usrid = UserId}) ->
	Res = k_aaa_api:del_customer_user(CstId, UserId),
	{ok, {result, Res}, State}.

terminate(_Req, _State = #state{}) ->
    ok.

%%% Local functions

%% convert "transmitter,receiver,transceiver"
%% to [transmitter, receiver, transmitter]
convert(SmppTypesString) ->
	Tokens = string:tokens(SmppTypesString, ","),
	convert(Tokens, []).

convert([], Acc) ->
	Acc;
convert([Type | Rest], Acc) ->
	case Type of
		"transmitter" -> convert(Rest, [transmitter | Acc]);
		"receiver" -> convert(Rest, [receiver | Acc]);
		"transceiver" -> convert(Rest, [transceiver | Acc]);
		_Any -> convert(Rest, Acc)
	end.
