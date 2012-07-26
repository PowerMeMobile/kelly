
-module(gen_cowboy_restful).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-export([behaviour_info/1]).

-include_lib("k_common/include/logging.hrl").

-record(state, {
	handler :: atom(),
	handler_state :: term(),
	handler_params :: {mandatory, binary()} | {optional, binary()},
	rest :: [{any(), any()}]
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
			?log_debug("Unexpected error: ~p", [Error]),
			Error
	end.

handle(Req, State = #state{
		%% handler_state = HandlerState,
		handler_params = Params
		}) ->
	{Method, _} = cowboy_http_req:method(Req),
	ReqPropList =
		case Method of
			Method when Method == 'POST'
						orelse
						Method == 'PUT' ->
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
			process_req(Req, State#state{rest = RestParams});
		{exception, Code, Variables} ->
			exception(Code, Variables, Req, State)
	end.

process_req(Req, State = #state{
		handler = Handler,
		handler_state = HandlerState,
		rest = RestParams
		}) ->
	{Suff, _} = cowboy_http_req:qs_val(<<"view">>, Req),
	case Handler:handle(Req, RestParams, HandlerState) of
		{http_code, Code, HandlerNewState} ->
			http_code(Code, Req, State#state{handler_state = HandlerNewState});
		{http_code, Code, Response, HandlerNewState} ->
			{ok, Body} = k_http_api_converter:process(Response, Suff),
			http_code(Code, Body, Req, State#state{handler_state = HandlerNewState});
		{exception, Code, Variables, HandlerNewState} ->
			exception(Code, Variables, Req, State#state{handler_state = HandlerNewState});
		{ok, Response, HandlerNewState} ->
			{ok, Body} = k_http_api_converter:process(Response, Suff),
			http_code(200, Body, Req, State#state{handler_state = HandlerNewState});
		Any ->
			?log_warn("Unexpected result: ~p", [Any]),
			http_code(500, Req, State)
	end.


terminate(Req, _State = #state{handler = Handler, handler_state = HandlerState}) ->
	ok = Handler:terminate(Req, HandlerState).

%% Local Functions

parse_params(Params, ReqPropList) ->
	parse_params(lists:reverse(tuple_to_list(Params)), [], ReqPropList).

parse_params([RecName | _Tail], Accum, _ReqPropList) when is_atom(RecName) ->
	{ok, list_to_tuple([RecName | Accum])};

parse_params([{mandatory, Pattern, Type} | Tail], Accum, ReqPropList) ->
	case proplists:get_value(Pattern, ReqPropList, undefined) of
		undefined ->
			?log_debug("Bad request. Missing mandatory parameter [~p].", [binary_to_list(Pattern)]),
			{exception, 'svc0005', [Pattern]};
		Value ->
			try
				Converted = convert(Value, Type),
				parse_params(Tail, [Converted | Accum], ReqPropList)
			catch
				_:_ ->
					?log_error("Invalid [~s] parameter value", [binary_to_list(Pattern)]),
					{exception, 'svc0001', [Pattern]}
			end
	end;

parse_params([{optional, Pattern, Type} | Tail], Accum, ReqPropList) ->
	case proplists:get_value(Pattern, ReqPropList, undefined) of
		undefined ->
			parse_params(Tail, [undefined | Accum], ReqPropList);
		Value ->
			try
				Converted = convert(Value, Type),
				parse_params(Tail, [Converted | Accum], ReqPropList)
			catch
				_:_ ->
					?log_error("Invalid [~s] parameter value", [binary_to_list(Pattern)]),
					{exception, 'svc0001', [Pattern]}
			end
	end.

convert(Any, boolean) ->
	convert_boolean(Any);
convert(undefined, _Type) ->
	undefined;
convert(UUID, string_uuid) ->
	UUIDstr = binary_to_list(UUID),
	case k_uuid:is_valid(UUIDstr) of
		true ->
			UUIDstr;
		false ->
			erlang:error(bad_uuid)
	end;
convert(Value, binary) ->
	Value;
convert(Value, list) ->
	binary_to_list(Value);
convert(Value, integer) ->
	list_to_integer(binary_to_list(Value)).

convert_boolean(<<"true">>) ->
	true;
convert_boolean(<<"false">>) ->
	false;
convert_boolean(Any) ->
	erlang:error({not_boolean, Any}).
%% ===================================================================
%% HTTP Response Codes
%% ===================================================================

http_code(Code, Req, State) ->
	http_code(Code, undefined, Req, State).

http_code(500, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"Internal Server Error">>),
	http_reply(500, [], Body, Req, State);
http_code(400, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"Bad request">>),
	http_reply(400, [], Body, Req, State);
http_code(401, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"Authentication failure, check your authentication details">>),
	Headers = [{'Www-Authenticate', <<"Basic">>}],
	http_reply(401, Headers, Body, Req, State);
http_code(404, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"Not found: mistake in the host or path of the service URI">>),
	http_reply(404, [], Body, Req, State);
http_code(201, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"Created">>),
	http_reply(201, [], Body, Req, State);
http_code(200, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"OK">>),
	http_reply(200, [], Body, Req, State).

http_reply(Code, Headers, Body, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(Code, Headers, Body, Req),
	{ok, Req2, State}.

resolve_body(undefined, DefaultBody) ->
	DefaultBody;
resolve_body(ExternalBody, _DefaultBody) ->
	ExternalBody.


%% ===================================================================
%% Exceptions
%% ===================================================================

-spec exception(ExceptionTag :: atom(), Variables :: [term()], Req :: term(), State :: term()) ->
	{ok, Req2 :: term(), State :: term()}.
exception(Code, Variables, Req, State) ->
	{ok, Body, HttpCode} = exception_body_and_code(Code, Variables),
	ContentType = <<"application/json">>,
	Headers = [{'Content-Type', ContentType}],
	http_reply(HttpCode, Headers, Body, Req, State).

%% Service exceptions

exception_body_and_code('svc0001', Variables) ->
	MessageID = <<"SVC0001">>,
	Text = <<"Invalid input %1 parameter value.">>,
	1 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0002', Variables) ->
	MessageID = <<"SVC0002">>,
	Text = <<"Invalid input %1 parameter value, valid values are %2">>,
	2 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0003', Variables) ->
	MessageID = <<"SVC0003">>,
	Text = <<"Resource not found.">>,
	0 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 404};

exception_body_and_code('svc0004', Variables) ->
	MessageID = <<"SVC0004">>,
	Text = <<"Resource already exist.">>,
	0 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0005', Variables) ->
	MessageID = <<"SVC0005">>,
	Text = <<"Missing mandatory %1 parameter.">>,
	1 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code(Exception, _Variables) ->
	?log_error("", []),
	{error, {no_such_exception, Exception}}.


exception_body(MessageID, Text, Variables) ->
	Body = jsx:term_to_json([{<<"request_error">>, [{<<"service_exception">>, [
													{<<"message_id">>, MessageID},
													{<<"text">>, Text},
													{<<"variables">>, Variables}
													]
											}]
						}]),
	{ok, Body}.
