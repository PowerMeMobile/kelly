
-module(gen_cowboy_crud).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-export([behaviour_info/1]).

-include_lib("k_common/include/logging.hrl").
-include("crud.hrl").

-record(addr, {
	addr :: string(),
	ton :: integer(),
	npi :: integer()
}).

-record(state, {
	handler :: atom(),
	req :: term(),
	handler_spec :: #specs{},
	method_spec :: #method_spec{},
	path :: list(),
	req_params :: [] | [{binary(), binary()}],
	handler_params :: [] | [{atom(), term()}],
	view :: term(),
	handler_func :: create | read | update | delete | index
}).

behaviour_info(callbacks) ->
	[
		{init, 0},
		{create, 1},
		{read, 1},
		{update, 1},
		{delete, 1}
	].

%% ===================================================================
%% Cowboy Callback Functions
%% ===================================================================

init({tcp, http}, Req, [Handler]) ->
	?log_debug("Req: ~p", [Req]),
	?log_debug("Handler: ~p", [Handler]),
	{View, _} = cowboy_http_req:qs_val(<<"view">>, Req),
	{ok, Req, #state{view = View, handler = Handler}}.

handle(Req, State = #state{handler = Handler}) ->
	{ok, HandlerSpec} = Handler:init(),
	{Path, _} = cowboy_http_req:path(Req),
	{Method, _} = cowboy_http_req:method(Req),
	ReqParameters = get_requests_parameters(Method, Req),
	?log_debug("ReqParameters: ~p", [ReqParameters]),
	NewState = State#state{
		req = Req,
		handler_spec = HandlerSpec,
		path = Path,
		req_params = ReqParameters},
	get_method_spec(Method, HandlerSpec, NewState).

terminate(Req, #state{}) ->
	ok.

%% ===================================================================
%% Local Functions
%% ===================================================================

get_method_spec('GET', #specs{read = Spec}, State) when Spec =/= undefined ->
	process_path(State#state{method_spec = Spec, handler_func = read});
get_method_spec('POST', #specs{create = Spec}, State) when Spec =/= undefined ->
	process_path(State#state{method_spec = Spec, handler_func = create});
get_method_spec('PUT', #specs{update = Spec}, State) when Spec =/= undefined ->
	process_path(State#state{method_spec = Spec, handler_func = update});
get_method_spec('DELETE', #specs{delete = Spec}, State) when Spec =/= undefined ->
	process_path(State#state{method_spec = Spec, handler_func = delete});
get_method_spec(Method, _, State = #state{req = Req}) ->
	?log_warn("[~p] method not supported", [Method]),
	exception('svc0006', [Method], Req, State).

process_path(State = #state{path = ReqPath, method_spec = SpecList}) when is_list(SpecList) ->
	?log_debug("ReqPath: ~p", [ReqPath]),
	?log_debug("Method spec: ~p", [SpecList]),
	[Spec] = lists:filter(fun(#method_spec{path = Path}) ->
		length(ReqPath) == length(Path)
	end, SpecList),
	#method_spec{path = MethodPath} = Spec,
	process_path(ReqPath, MethodPath, [], State#state{method_spec = Spec});
process_path(State = #state{path = ReqPath, method_spec = Spec}) ->
	#method_spec{path = MethodPath} = Spec,
	process_path(ReqPath, MethodPath, [], State).

process_path([], [], Acc, State = #state{req_params = Params}) ->
	?log_debug("Req params with path: ~p", [Acc ++ Params]),
	process_method_params(State#state{req_params = Acc ++ Params});
process_path([Value | TailReqPath], [Name | TailMethodPath], Acc, State)
		when is_atom(Name) ->
	NewAcc = [{atom_to_binary(Name, utf8), Value}] ++ Acc,
	process_path(TailReqPath, TailMethodPath, NewAcc, State);
process_path([Elem | TailReqPath], [Elem | TailMethodPath], Acc, State) ->
	process_path(TailReqPath, TailMethodPath, Acc, State);
process_path(_, _, _, State = #state{req = Req}) ->
	?log_warn("Error in path", []),
	exception('svc0007', [], Req, State).


process_method_params(State = #state{method_spec = Spec, req_params = ReqParamsPL}) ->
	#method_spec{params = ParamsSpec} = Spec,
	process_method_params(ParamsSpec, ReqParamsPL, [], State).

process_method_params([], _, Acc, State = #state{}) ->
	?log_debug("Processed parameters: ~p", [Acc]),
	process_req(State#state{handler_params = Acc});
process_method_params([ParamSpec | Tail], ReqParamsPL, Acc, State = #state{req = Req}) ->
	?log_debug("ParamSpec: ~p", [ParamSpec]),
	case get_parameter(ParamSpec, ReqParamsPL) of
		{ok, KeyValue} ->
			process_method_params(Tail, ReqParamsPL, [KeyValue | Acc], State);
		{error, missing, Name} ->
			exception('svc0005', [Name], Req, State);
		{error, invalid, Name} ->
			exception('svc0001', [Name], Req, State);
		{error, invalid, Name, Range} ->
			exception('svc0002', [Name, Range], Req, State);
		{error, disabled, <<"rps">>} ->
			exception('svc0008', [], Req, State)
	end.

get_parameter(Spec = #param{name = Name, repeated = true}, ReqParamsPL) ->
	ValuesDelimited = proplists:get_value(atom_to_binary(Name, utf8), ReqParamsPL),
	case ValuesDelimited of
		undefined -> {ok, {Name, []}};
		_ ->
			Values = binary:split(ValuesDelimited, [<<";">>], [global, trim]),
			validate_repeated(Values, [], Spec)
	end;
get_parameter(Spec = #param{name = Name, repeated = false}, ReqParamsPL) ->
	Value = proplists:get_value(atom_to_binary(Name, utf8), ReqParamsPL),
	validate(Value, Spec).

validate_repeated([], [], Spec = #param{mandatory = true}) ->
	{error, missing, atom_to_binary(Spec#param.name, utf8)};
validate_repeated([], Acc, Spec) ->
	{ok, {Spec#param.name, Acc}};
validate_repeated([RawValue | Tail], Acc, Spec) ->
	case validate(RawValue, Spec) of
		{ok, {_Key, Value}} ->
			validate_repeated(Tail, [Value | Acc], Spec);
		Any ->
			Any
	end.

validate(Value, Spec = #param{mandatory = true}) when Value == [] orelse Value == undefined ->
	?log_warn("Bad request. Missing mandatory parameter [~p].", [Spec#param.name]),
	{error, missing, Spec#param.name};
validate(Value, Spec) ->
	try
		Converted = convert(Value, Spec#param.type),
		{ok, {Spec#param.name, Converted}}
	catch
		error:disabled ->
			?log_warn("Disabled parameter found [~p]", [Spec#param.name]),
			{error, disabled, atom_to_binary(Spec#param.name, utf8)};
		error:_ ->
			?log_warn("Invalid [~s] parameter value", [Spec#param.name]),
			{error, invalid, atom_to_binary(Spec#param.name, utf8)}
	end.


process_req(State = #state{req = Req, handler_params = Params, view = V, handler = Handler, handler_func = Function}) ->
	case Handler:Function(Params) of
		{http_code, Code} ->
			http_code(Code, Req, State);
		{http_code, Code, Response} ->
			{ok, Body} = k_http_api_converter:process(Response, V),
			http_code(Code, Body, Req, State);
		{exception, Code} ->
			exception(Code, [], Req, State);
		{exception, Code, Variables} ->
			exception(Code, Variables, Req, State);
		{ok, Response} ->
			{ok, Body} = k_http_api_converter:process(Response, V),
			http_code(200, Body, Req, State);
		Any ->
			?log_warn("Unexpected result: ~p", [Any]),
			http_code(500, Req, State)
	end.

convert(undefined, _Type) ->
	undefined;
convert(State, customer_state) ->
	case State of
		<<"0">> -> 0;
		<<"1">> -> 1
	end;
convert(SMPPType, smpp_type) ->
	convert_smpp_type(SMPPType);
convert(Value, addr) ->
	?log_debug("Addr: ~p", [Value]),
	decode_address(Value);
convert(_Value, disabled) ->
	erlang:error(disabled);
convert(Any, boolean) ->
	convert_boolean(Any);
convert(UUID, binary_uuid) ->
	UUIDstr = binary_to_list(UUID),
	UUIDbin = k_uuid:to_binary(UUIDstr),
	validate_uuid(UUIDbin);
convert(UUID, string_uuid) ->
	UUIDstr = binary_to_list(UUID),
	validate_uuid(UUIDstr);
convert(Value, binary) ->
	Value;
convert(Value, string) ->
	binary_to_list(Value);
convert(Value, integer) ->
	list_to_integer(binary_to_list(Value)).

convert_boolean(<<"true">>) ->
	true;
convert_boolean(<<"false">>) ->
	false;
convert_boolean(Any) ->
	erlang:error({not_boolean, Any}).

validate_uuid(UUID) ->
	case k_uuid:is_valid(UUID) of
		true ->
			UUID;
		false ->
			erlang:error(bad_uuid)
	end.

%% convert "addr,ton,npi" to #addr{addr, ton, npi}
decode_address(AddrBin) ->
	AddrString = binary_to_list(AddrBin),
	[Addr, Ton, Npi] = string:tokens(AddrString, ","),
	#addr{
		addr = list_to_binary(Addr),
		ton = list_to_integer(Ton),
		npi = list_to_integer(Npi)
	}.

convert_smpp_type(Type) ->
	case Type of
		<<"transmitter">> -> transmitter;
		<<"receiver">> -> receiver;
		<<"transceiver">> -> transceiver
	end.

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
http_code(204, ExtBody, Req, State) ->
	Body = resolve_body(ExtBody, <<"No content">>),
	http_reply(204, [], Body, Req, State);
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
	?log_debug("Code: ~p, Variables: ~p", [Code, Variables]),
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

exception_body_and_code('svc0006', Variables) ->
	MessageID = <<"SVC0006">>,
	Text = <<"Http method %1 not supported.">>,
	1 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0007', Variables) ->
	MessageID = <<"SVC0007">>,
	Text = <<"Error in path">>,
	0 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0008', Variables) ->
	MessageID = <<"SVC0008">>,
	Text = <<"Customer's \"rps\" setting is disabled. See http://extranet.powermemobile.com/issues/17465 for detail.">>,
	0 = length(Variables),
	{ok, Body} = exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code(Exception, _Variables) ->
	?log_error("", []),
	{error, {no_such_exception, Exception}}.


exception_body(MessageID, Text, Variables) ->
	Body = [{<<"request_error">>, [{<<"service_exception">>, [
													{<<"message_id">>, MessageID},
													{<<"text">>, Text},
													{<<"variables">>, Variables}
													]
											}]
						}],
	{ok, Json} = k_http_api_converter:process(Body, <<"json">>),
	?log_debug("Exception json body: ~p", [Body]),
	{ok, Json}.

get_requests_parameters(Method, Req) ->
	Parameters =
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
	Parameters.
