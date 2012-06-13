-module(gen_cowboy_restful).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-export([behaviour_info/1]).

-include_lib("k_common/include/logging.hrl").

-record(state, {
	handler :: atom(),
	handler_state :: term(),
	handler_params :: {mandatory, binary()} | {optional, binary()}
}).

behaviour_info(callbacks) ->
	[
		{init, 3},
		{handle, 3},
		{terminate, 2}
	].

init({tcp, http}, Req, [Opts]) ->
	?log_debug("Req: ~p", [Req]),
	Handler = Opts,
	{Method, _} = cowboy_http_req:method(Req),
	{Path, _} = cowboy_http_req:path(Req),
	InitResult = Handler:init(Req, Method, Path),
	?log_debug("InitResult: ~p", [InitResult]),
	case InitResult of
		{ok, Params, HandlerState} ->
			{ok, Req, #state{
					handler = Handler,
					handler_params = Params,
					handler_state = HandlerState
					}};
		Error ->
			Error
	end.

handle(Req, State = #state{
		handler = Handler,
		handler_state = HandlerState,
		handler_params = Params
		}) ->
	{Method, _} = cowboy_http_req:method(Req),
	ReqPropList =
		case Method of
			'POST' ->
				{BodyQs, _} = cowboy_http_req:body_qs(Req),
				{QsVals, _} = cowboy_http_req:qs_vals(Req),
				BodyQs ++ QsVals;
			_Any ->
				{QsVals, _} = cowboy_http_req:qs_vals(Req),
				QsVals
		end,
	?log_debug("ReqPropList: ~p", [ReqPropList]),

	case parse_params(Params, ReqPropList) of
		{ok, RestParams} ->
			{ok, Response, HandlerStateNew} =
				Handler:handle(Req, RestParams, HandlerState),
			% ?log_debug("Response: ~p", [Response]),

			{Suff, _} = cowboy_http_req:qs_val(<<"view">>, Req),

			{ok, RespBin} = k_http_api_converter:process(Response, Suff),

			{ok, Req2} = cowboy_http_req:reply(200, [], RespBin, Req),

			{ok, Req2, State#state{handler_state=HandlerStateNew}};
	 	{error, Error} ->
	 		ErrorResponse = list_to_binary(io_lib:format("~p", [Error])),
	 		{ok, Req2} = cowboy_http_req:reply(400, [], ErrorResponse, Req),
	 		{ok, Req2, State#state{handler_state=HandlerState}}
	end.

terminate(Req, _State = #state{handler=Handler, handler_state=HandlerState}) ->
	ok = Handler:terminate(Req, HandlerState),
	ok.

%% Local Functions

parse_params(Params, ReqPropList) ->
	parse_params(lists:reverse(tuple_to_list(Params)), [], ReqPropList).

parse_params([RecName | _Tail], Accum, _ReqPropList) when is_atom(RecName) ->
	{ok, list_to_tuple([RecName | Accum])};

parse_params([{mandatory, Pattern, Type} | Tail], Accum, ReqPropList) ->
	case proplists:get_value(Pattern, ReqPropList, undefined) of
		undefined ->
			{error, "Mandatory parameter undefined: "
			++ binary_to_list(Pattern)};
		Value ->
			parse_params(Tail, [convert(Value, Type) | Accum], ReqPropList)
	end;

parse_params([{optional, Pattern, Type} | Tail], Accum, ReqPropList) ->
	Value = proplists:get_value(Pattern, ReqPropList, undefined),
	parse_params(Tail, [convert(Value, Type) | Accum], ReqPropList).

%% mathing for boolean values
convert(undefined, boolean) ->
	false;
convert(_Value, boolean) ->
	true;
%% mathing for optional values if undefined
convert(undefined, _Type) ->
	undefined;
%% mathing for mandatory values
convert(Value, binary) ->
	Value;
convert(Value, list) ->
	binary_to_list(Value);
%%% Roma. Here badarg exception may occure if Value contains a bad representation of an integer
convert(Value, integer) ->
	list_to_integer(binary_to_list(Value)).
