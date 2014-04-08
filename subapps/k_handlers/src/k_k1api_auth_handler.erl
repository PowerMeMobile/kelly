-module(k_k1api_auth_handler).

-export([process/1]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, Payload} = k_amqp_req:payload(Req),
    case adto:decode(#k1api_auth_request_dto{}, Payload) of
        {ok, AuthReq} ->
            process_auth_req(Req, AuthReq);
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            ?authentication_failed
    end.

%% ===================================================================
%% Internals
%% ===================================================================

process_auth_req(Req, AuthReq) ->
    case authenticate(AuthReq) of
        {allow, Customer = #customer{}} ->
            {ok, Response} = build_customer_response(AuthReq, Customer),
            step(is_reply_to_defined, Req, Response);
        {deny, Reason} ->
            {ok, Response} = build_error_response(AuthReq, {deny, Reason}),
            ?log_notice("Auth denied: ~p", [Reason]),
            step(is_reply_to_defined, Req, Response);
        {error, Reason} ->
            {ok, Response} = build_error_response(AuthReq, {error, Reason}),
            ?log_error("Auth error: ~p", [Reason]),
            step(is_reply_to_defined, Req, Response)
    end.

-spec authenticate(#k1api_auth_request_dto{}) ->
    {allow, #customer{}} |
    {error, term()} |
    {deny, no_such_customer} |
    {deny, password} |
    {deny, connection_type}.
authenticate(AuthReq = #k1api_auth_request_dto{
    customer_id = CustomerId,
    user_id = UserId
}) ->
    ?log_debug("Got auth request: ~p", [AuthReq]),

    case k_aaa:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case k_aaa:get_customer_user(Customer, UserId) of
                {ok, User = #user{}} ->
                    ?log_debug("User found: ~p", [User]),
                    Checks = [
                        fun check_password/2,
                        fun check_conn_type/2
                    ],
                    perform_checks(AuthReq, User, Checks, Customer);
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

check_password(#k1api_auth_request_dto{password = PwHash}, #user{password = PwHash}) ->
    allow;
check_password(#k1api_auth_request_dto{password = Pw}, #user{password = PwHash}) ->
    case ac_hexdump:binary_to_hexdump(crypto:hash(md5, Pw), to_lower) =:= PwHash of
        true ->
            allow;
        _ ->
            {deny, password}
    end.

check_conn_type(AuthReq, User) ->
    ConnType = AuthReq#k1api_auth_request_dto.connection_type,
    ConnTypes = User#user.connection_types,
    case lists:member(ConnType, ConnTypes) of
        true ->
            allow;
        false ->
            {deny, connection_type}
    end.

perform_checks(_, _, [], Customer) ->
    ?log_debug("Auth allowed", []),
    {allow, Customer};
perform_checks(AuthReq, User = #user{}, [Check | Checks], Customer) ->
    case Check(AuthReq, User) of
        allow ->
            perform_checks(AuthReq, User, Checks, Customer);
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
        customer_id = CustomerId,
        priority = _Prio,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
        networks = NetworkIds,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = _DV,
        max_validity = MV,
        pay_type = PayType
    } = Customer,

    {Networks, Providers} = lists:foldl(fun(NetworkId, {Ns, Ps})->
        {ok, Network} = k_config:get_network(NetworkId),
        #network{
            country_code = CC,
            numbers_len = NL,
            prefixes = Pref,
            provider_id = ProviderId
        } = Network,
        NetworkDTO = #network_dto{
            id = NetworkId,
            country_code = CC,
            numbers_len = NL,
            prefixes = Pref,
            provider_id = ProviderId
        },

        {ok, Provider} = k_config:get_provider(ProviderId),
        #provider{
            gateway_id = GatewayId,
            bulk_gateway_id = BulkGatewayId,
            receipts_supported = RS
        } = Provider,
        ProviderDTO = #provider_dto{
            id = ProviderId,
            gateway_id = GatewayId,
            bulk_gateway_id = BulkGatewayId,
            receipts_supported = RS
        },

        %% check for Providers dublications
        case lists:member(ProviderDTO, Ps) of
            true ->
                {[NetworkDTO | Ns], Ps};
            false ->
                {[NetworkDTO | Ns], [ProviderDTO | Ps]}
        end
    end, {[], []}, NetworkIds),

    CustomerDTO = #k1api_auth_response_customer_dto{
        uuid = CustomerUUID,
        id = CustomerId,
        pay_type = PayType,
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

    ResponseDTO = #k1api_auth_response_dto{
        id = ReqId,
        result = {customer, CustomerDTO}
    },
    ?log_debug("Built response: ~p", [ResponseDTO]),
    {ok, ResponseDTO}.

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
            ?log_warn("Unexpected auth response encode error: ~p", [Error]),
            Error
    end.
