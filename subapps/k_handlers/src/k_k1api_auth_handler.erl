-module(k_k1api_auth_handler).

-export([process/1]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%% -record('basic.deliver', {consumer_tag, delivery_tag, redelivered = false, exchange, routing_key}).
			 %% #amqp_msg{props = #'P_basic'{}, payload = Content}},
%% -record(amqp_msg, {props = #'P_basic'{}, payload = <<>>}).
%% -record('P_basic', {content_type, content_encoding, headers, delivery_mode, priority, correlation_id, reply_to, expiration, message_id, timestamp, type, user_id, app_id, cluster_id}).

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
	?log_debug("Got k1api auth request", []),
	{ok, Payload} = k_amqp_req:payload(Req),
	case adto:decode(#k1api_auth_request_dto{}, Payload) of
		{ok, AuthRequest} ->
			process_auth_req(Req, AuthRequest);
		{error, Error} ->
			?log_error("k1api auth request decode error: ~p", [Error]),
			?authentication_failed
	end.

%% ===================================================================
%% Internals
%% ===================================================================

process_auth_req(Req, AuthRequest) ->
	case authenticate(AuthRequest) of
		{allow, Customer = #customer{}} ->
			{ok, Response} = build_customer_response(AuthRequest, Customer),
			step(is_reply_to_defined, Req, Response);
		{deny, Reason} ->
            {ok, Response} = build_error_response(AuthRequest, {deny, Reason}),
			?log_notice("k1api authentication denied: ~p", [Reason]),
			step(is_reply_to_defined, Req, Response);
		{error, Reason} ->
            {ok, Response} = build_error_response(AuthRequest, {error, Reason}),
			?log_error("authentication error: ~p", [Reason]),
			step(is_reply_to_defined, Req, Response)
	end.

-spec authenticate(#k1api_auth_request_dto{}) ->
	{allow, #customer{}} |
	{error, term()} |
	{deny, no_such_customer} |
	{deny, password} |
	{deny, connection_type}.
authenticate(BindReq = #k1api_auth_request_dto{
	customer_id = CustomerId,
	user_id = UserId
}) ->
	?log_debug("Got k1api auth request: ~p", [BindReq]),

	case k_aaa:get_customer_by_id(CustomerId) of
		{ok, Customer} ->
			?log_debug("Customer found: ~p", [Customer]),
			case k_aaa:get_customer_user(Customer, UserId) of
				{ok, User = #user{}} ->
					?log_debug("User found: ~p", [User]),
					Checks = [
						fun check_password/2,
						fun check_bind_type/2
					],
					perform_checks(BindReq, User, Checks, Customer);
				{error, no_entry} ->
					?log_debug("User not found.", []),
					{deny, no_such_customer};
				Error ->
					?log_error("Unexpected error: ~p", [Error]),
					Error
			end;
		Any ->
			?log_debug("Customer not found.", []),
			Any
	end.

check_password(#k1api_auth_request_dto{password = Pw}, #user{password = PwHash}) ->
	case base64:encode(crypto:sha(Pw)) =:= PwHash of
		true ->
			allow;
		_ ->
			{deny, password}
	end.

check_bind_type(#k1api_auth_request_dto{}, #user{bind_types = BindTypes}) ->
	ReqBindType = oneapi,
	case lists:any(fun(T) -> T =:= ReqBindType end, BindTypes) of
		true ->
			allow;
		_ ->
			{deny, connection_type}
	end.

perform_checks(_, _, [], Customer) ->
	?log_debug("k1api auth allowed", []),
	{allow, Customer};
perform_checks(BindReq, User = #user{}, [Check | SoFar], Customer) ->
	case Check(BindReq, User) of
		allow ->
			perform_checks(BindReq, User, SoFar, Customer);
		{deny, DenyReason} ->
			{deny, DenyReason}
	end.

build_customer_response(Request, Customer) ->
    #k1api_auth_request_dto{
	    id = ReqId,
    	customer_id = CustomerId
    } = Request,

    #customer{
	    customer_uuid = CustomerUUID,
		billing_type = BillingType,
		allowed_sources = AllowedSources,
		default_source = DefaultSource,
		networks = NtwIdList,
		default_provider_id = DP,
		receipts_allowed = RA,
		no_retry = NR,
		default_validity = _DV,
		max_validity = MV
    } = Customer,

	{Networks, Providers} = lists:foldl(fun(NetworkId, {N, P})->
		%%%NETWORK SECTION%%%
		{ok, Network} = k_config:get_network(NetworkId),
		#network{
			country_code = CC,
			numbers_len = NL,
			prefixes = Pref,
			provider_id = ProviderId
			} = Network,
		NNew = #network_dto{
			id = NetworkId,
			country_code = CC,
			numbers_len = NL,
			prefixes = Pref,
			provider_id = ProviderId
		},

		%%% PROVIDER SECTION %%%
		{ok, Provider} = k_config:get_provider(ProviderId),
		#provider{
			gateway = Gateway,
			bulk_gateway = BGateway,
			receipts_supported = RS
		} = Provider,
		PNew = #provider_dto{
			id = ProviderId,
			gateway = Gateway,
			bulk_gateway = BGateway,
			receipts_supported = RS
		},
		%%% check for Provider dublications %%%
		case lists:member(PNew, P) of
			true ->
				{[NNew | N], P};
			false ->
				{[NNew | N], [PNew | P]}
		end
	end, {[], []}, NtwIdList),

    CustomerDTO = #k1api_auth_response_customer_dto{
		uuid = CustomerUUID,
		id = CustomerId,
		billing_type = BillingType,
		allowed_sources = AllowedSources,
		default_source = DefaultSource,
		networks = Networks,
		providers = Providers,
		default_provider_id = DP,
		receipts_allowed = RA,
		no_retry = NR,
		default_validity = MV, %% fake
		max_validity = MV
    },

	Response = #k1api_auth_response_dto{
		id = ReqId,
        result = {customer, CustomerDTO}
	},
	?log_debug("Built response: ~p", [Response]),
	{ok, Response}.

build_error_response(Request, {deny, Reason}) ->
    #k1api_auth_request_dto{
	    id = ReqId
    } = Request,

	Response = #k1api_auth_response_dto{
		id = ReqId,
        result = {error, "Request denied: " ++ atom_to_list(Reason)}
	},

    ?log_debug("Built response: ~p", [Response]),
	{ok, Response};
build_error_response(Request, {error, Reason}) ->
    #k1api_auth_request_dto{
	    id = ReqId
    } = Request,

	Response = #k1api_auth_response_dto{
		id = ReqId,
        result = {error, "Request error: " ++ atom_to_list(Reason)}
	},

    ?log_debug("Built response: ~p", [Response]),
	{ok, Response}.


step(is_reply_to_defined, Req, DTO) ->
	case k_amqp_req:reply_to(Req) of
		{ok, undefined} ->
			% reply_to is undefined, sekip req
			?log_warn("reply_to is undefined. skip request", []),
			{ok, []};
		{ok, _ReplyTo} ->
			% reply_to is defined, reply
			step(reply, Req, DTO)
	end;

step(reply, Req, Response) ->
	case adto:encode(Response) of
		{ok, Binary} ->
			{ok, ReplyTo} = k_amqp_req:reply_to(Req),
			Reply = #worker_reply{
				reply_to = ReplyTo,
				content_type = <<"OneAPIAuthResponse">>,
				payload = Binary
            },
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api auth response encode error: ~p", [Error]),
	   		Error
	end.
