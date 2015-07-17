-module(k_amqp_auth_handler).

-export([start_link/0]).
-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Queue} = application:get_env(k_handlers, kelly_auth_queue),
    rmql_rpc_server:start_link(?MODULE, Queue, fun ?MODULE:process/2).

%% ===================================================================
%% Internal
%% ===================================================================

-spec process(binary(), binary()) -> {binary(), binary()} | {ok, []}.
process(<<"BindRequest">>, ReqBin) ->
    RespCT = <<"BindResponse">>,
    case adto:decode(#funnel_auth_request_dto{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            CustomerId = AuthReq#funnel_auth_request_dto.customer_id,
            UserId     = AuthReq#funnel_auth_request_dto.user_id,
            Password   = AuthReq#funnel_auth_request_dto.password,
            ConnType   = AuthReq#funnel_auth_request_dto.type,
            ReqId      = AuthReq#funnel_auth_request_dto.connection_id,
            case authenticate(CustomerId, UserId, Password, ConnType) of
                {allow, Customer = #customer{}, #user{}} ->
                    {ok, Networks, Providers} = get_networks_and_providers(Customer),
                    Features = get_features(UserId, Customer),
                    {ok, Response} = build_auth_response(RespCT,
                        ReqId, Customer, UserId, Networks, Providers, Features),
                    ?log_debug("Auth allowed", []),
                    encode_response(RespCT, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(RespCT, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {error, Reason}),
                    ?log_error("Auth error: ~p", [Reason]),
                    encode_response(RespCT, Response)
            end;
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            noreply
    end;
process(<<"AuthReqV1">>, ReqBin) ->
    RespCT = <<"AuthRespV1">>,
    case adto:decode(#auth_req_v1{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            CustomerId = AuthReq#auth_req_v1.customer_id,
            UserId     = AuthReq#auth_req_v1.user_id,
            Password   = AuthReq#auth_req_v1.password,
            Interface  = AuthReq#auth_req_v1.interface,
            ReqId      = AuthReq#auth_req_v1.req_id,
            case authenticate(CustomerId, UserId, Password, Interface) of
                {allow, Customer = #customer{}, #user{}} ->
                    {ok, Networks, Providers} = get_networks_and_providers(Customer),
                    Features = get_features(UserId, Customer),
                    {ok, Response} = build_auth_response(RespCT,
                        ReqId, Customer, UserId, Networks, Providers, Features),
                    ?log_debug("Auth allowed", []),
                    encode_response(RespCT, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(RespCT, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {error, Reason}),
                    ?log_error("Auth error: ~p", [Reason]),
                    encode_response(RespCT, Response)
            end;
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            noreply
    end;
process(<<"AuthReqV2">>, ReqBin) ->
    RespCT = <<"AuthRespV2">>,
    case adto:decode(#auth_req_v2{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            AuthData  = AuthReq#auth_req_v2.auth_data,
            Interface = AuthReq#auth_req_v2.interface,
            ReqId     = AuthReq#auth_req_v2.req_id,
            case authenticate_by(AuthData, Interface) of
                {allow, Customer = #customer{}, User = #user{}} ->
                    {ok, Networks, Providers} = get_networks_and_providers(Customer),
                    Features = get_features(User#user.id, Customer),
                    {ok, Response} = build_auth_response(RespCT,
                        ReqId, Customer, User#user.id, Networks, Providers, Features),
                    ?log_debug("Auth allowed", []),
                    encode_response(RespCT, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(RespCT, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(RespCT,
                        ReqId, {error, Reason}),
                    ?log_error("Auth error: ~p", [Reason]),
                    encode_response(RespCT, Response)
            end;
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            noreply
    end;
process(ContentType, ReqBin) ->
    ?log_error("Got unknown auth request: ~p ~p", [ContentType, ReqBin]),
    noreply.

-spec authenticate_by(#auth_credentials{} | #auth_email{} | #auth_msisdn{}, atom()) ->
    {allow, #customer{}, #user{}} |
    {error, term()} |
    {deny, unknown_customer} |
    {deny, unknown_user} |
    {deny, wrong_password} |
    {deny, wrong_interface} |
    {deny, blocked_customer} |
    {deny, blocked_user} |
    {deny, deactivated_customer} |
    {deny, deactivated_user} |
    {deny, credit_limit_exceeded}.
authenticate_by(C = #auth_credentials{}, Interface) ->
    CustomerId = C#auth_credentials.customer_id,
    UserId     = C#auth_credentials.user_id,
    Password   = C#auth_credentials.password,
    authenticate(CustomerId, UserId, Password, Interface);
authenticate_by(#auth_email{email = Email}, Interface) ->
    authenticate_by_email(Email, Interface);
authenticate_by(#auth_msisdn{msisdn = Msisdn}, Interface) ->
    authenticate_by_msisdn(Msisdn, Interface).

authenticate(CustomerId, UserId, Password, Interface) ->
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case check_state(customer, Customer#customer.state) of
                allow ->
                    case k_storage_customers:get_user_by_id(Customer, UserId) of
                        {ok, User = #user{}} ->
                            ?log_debug("User found: ~p", [User]),
                            case check_password(Password, User#user.password) of
                                allow ->
                                    case check_state(user, User#user.state) of
                                        allow ->
                                            case check_interface(Interface, User#user.interfaces) of
                                                allow ->
                                                    case check_credit_limit(Customer) of
                                                        allow ->
                                                            {allow, Customer, User};
                                                        Deny ->
                                                            Deny
                                                    end;
                                                Deny ->
                                                    Deny
                                            end;
                                        Deny ->
                                            Deny
                                    end;
                                Deny ->
                                    Deny
                            end;
                        {error, no_entry} ->
                            ?log_debug("Customer id: ~p User id: ~p not found", [CustomerId, UserId]),
                            {deny, unknown_user};
                        {error, Error} ->
                            ?log_error("Unexpected error: ~p", [Error]),
                            {error, Error}
                    end;
                Deny ->
                    Deny
            end;
        {error, no_entry} ->
            ?log_info("Customer id: ~p not found", [CustomerId]),
            {deny, unknown_customer};
        {error, Error} ->
            ?log_error("Unexpected error: ~p.", [Error]),
            {error, Error}
    end.

authenticate_by_email(Email, Interface) ->
    case k_storage_customers:get_customer_by_email(Email) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case check_state(customer, Customer#customer.state) of
                allow ->
                    case k_storage_customers:get_user_by_email(Customer, Email) of
                        {ok, User = #user{}} ->
                            ?log_debug("User found: ~p", [User]),
                            case check_state(user, User#user.state) of
                                allow ->
                                    case check_interface(Interface, User#user.interfaces) of
                                        allow ->
                                            case check_credit_limit(Customer) of
                                                allow ->
                                                    {allow, Customer, User};
                                                Deny ->
                                                    Deny
                                            end;
                                        Deny ->
                                            Deny
                                    end;
                                Deny ->
                                    Deny
                            end;
                        {error, no_entry} ->
                            ?log_debug("User by email: ~p not found", [Email]),
                            {deny, unknown_user};
                        {error, Error} ->
                            ?log_error("Unexpected error: ~p", [Error]),
                            {error, Error}
                    end;
                Deny ->
                    Deny
            end;
        {error, no_entry} ->
            ?log_info("Customer by email: ~p not found", [Email]),
            {deny, unknown_customer};
        {error, Error} ->
            ?log_error("Unexpected error: ~p.", [Error]),
            {error, Error}
    end.

authenticate_by_msisdn(Msisdn, Interface) ->
    case k_storage_customers:get_customer_by_msisdn(Msisdn) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case check_state(customer, Customer#customer.state) of
                allow ->
                    case k_storage_customers:get_user_by_msisdn(Customer, Msisdn) of
                        {ok, User = #user{}} ->
                            ?log_debug("User found: ~p", [User]),
                            case check_state(user, User#user.state) of
                                allow ->
                                    case check_interface(Interface, User#user.interfaces) of
                                        allow ->
                                            case check_credit_limit(Customer) of
                                                allow ->
                                                    {allow, Customer, User};
                                                Deny ->
                                                    Deny
                                            end;
                                        Deny ->
                                            Deny
                                    end;
                                Deny ->
                                    Deny
                            end;
                        {error, no_entry} ->
                            ?log_debug("User by msisdn: ~p not found", [Msisdn]),
                            {deny, unknown_user};
                        {error, Error} ->
                            ?log_error("Unexpected error: ~p", [Error]),
                            {error, Error}
                    end;
                Deny ->
                    Deny
            end;
        {error, no_entry} ->
           ?log_info("Customer by msisdn: ~p not found", [Msisdn]),
           {deny, unknown_customer};
        {error, Error} ->
          ?log_error("Unexpected error: ~p", [Error])
    end.

check_password(PasswHash, PasswHash) ->
    allow;
check_password(Passw, PasswHash) ->
    Enc = fun(P) ->
        ac_hexdump:binary_to_hexdump(crypto:hash(md5, P), to_lower)
    end,
    Hashes = [Enc(P) || P <- [Passw, bstr:lower(Passw), bstr:upper(Passw)]],
    case lists:member(PasswHash, Hashes) of
        true ->
            allow;
        _ ->
            {deny, wrong_password}
    end.

check_interface(Interface, Interfaces) ->
    case lists:member(Interface, Interfaces) of
        true ->
            allow;
        false ->
            {deny, wrong_interface}
    end.

check_state(_, active) ->
    allow;
check_state(customer, blocked) ->
    {deny, blocked_customer};
check_state(user, blocked) ->
    {deny, blocked_user};
check_state(customer, deactivated) ->
    {deny, deactivated_customer};
check_state(user, deactivated) ->
    {deny, deactivated_user}.

check_credit_limit(Customer) ->
    Credit = Customer#customer.credit,
    CreditLimit = Customer#customer.credit_limit,
    case Credit =< -CreditLimit of
        true ->
            {deny, credit_limit_exceeded};
        false ->
            allow
    end.

get_networks_and_providers(Customer) ->
    CustomerUuid = Customer#customer.customer_uuid,
    case k_handlers_auth_cache:get(CustomerUuid) of
        {ok, {Ns, Ps}} ->
            {ok, Ns, Ps};
        {error, no_entry} ->
            {ok, Ns, Ps} = build_networks_and_providers(Customer),
            k_handlers_auth_cache:set(CustomerUuid, {Ns, Ps}),
            {ok, Ns, Ps}
    end.

build_networks_and_providers(Customer) ->
    NetworkMapId = Customer#customer.network_map_id,
    DefProvId = Customer#customer.default_provider_id,

    case k_storage_network_maps:get_network_map(NetworkMapId) of
        {ok, #network_map{network_ids = NetworkIds}} ->
            {Networks, Providers} = lists:foldl(fun(NetworkId, {Ns, Ps})->
                case k_storage_networks:get_network(NetworkId) of
                    {ok, Network} ->
                        ProviderId = Network#network.provider_id,
                        case k_storage_providers:get_provider(ProviderId) of
                            {ok, Provider} ->
                                {[Network | Ns], insert_if_not_member(Provider, Ps)};
                            {error, Error} ->
                                ?log_error("Get provider id: ~p from network id: ~p from map id: ~p failed with: ~p",
                                    [ProviderId, NetworkId, NetworkMapId, Error]),
                                {Ns, Ps}
                        end;
                    {error, Error} ->
                        ?log_error("Get network id: ~p from map id: ~p failed with: ~p",
                            [NetworkId, NetworkMapId, Error]),
                        {Ns, Ps}
                end
            end, {[], []}, NetworkIds),
            %% add default provider to providers list.
            case DefProvId of
                undefined ->
                    {ok, Networks, Providers};
                _ ->
                    case k_storage_providers:get_provider(DefProvId) of
                        {ok, DefaultProvider} ->
                            {ok, Networks, insert_if_not_member(DefaultProvider, Providers)};
                        {error, Error} ->
                            ?log_error("Get default provider id: ~p from customer id: ~p failed with: ~p",
                                [DefProvId, Customer#customer.customer_uuid, Error]),
                            %% TODO: this will fail on client on price calculation.
                            {ok, Networks, Providers}
                    end
            end;
        {error, Error} ->
            ?log_error("Get map id: ~p failed with: ~p", [NetworkMapId, Error]),
            {ok, [], []}
    end.

build_auth_response(<<"BindResponse">>, ReqId, Customer, _UserId, Networks, Providers, Features) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        priority = Prio,
        rps = RPS,
        originators = Originators,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = DV,
        max_validity = MV,
        pay_type = PayType
    } = Customer,

    CustomerDTO = #funnel_auth_response_customer_dto{
        uuid = CustomerUuid,
        id = CustomerId,
        priority = Prio,
        rps = RPS,
        allowed_sources = allowed_sources(Originators),
        default_source = default_source(Originators),
        networks = [network_to_dto(N) || N <- Networks],
        providers = [provider_to_dto(P) || P <- Providers],
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = DV,
        max_validity = MV,
        pay_type = PayType,
        features = [feature_to_dto(F) || F <- Features]
    },
    ResponseDTO = #funnel_auth_response_dto{
        connection_id = ReqId,
        result = {customer, CustomerDTO}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO};
build_auth_response(<<"AuthRespV1">>, ReqId, Customer, UserId, Networks, Providers, Features) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        originators = Originators,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = _DV,
        max_validity = MV,
        pay_type = PayType,
        credit = Credit,
        credit_limit = CreditLimit
    } = Customer,

    CustomerDTO = #auth_customer_v1{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        user_id = UserId,
        pay_type = PayType,
        credit = Credit + CreditLimit,
        allowed_sources = allowed_sources(Originators),
        default_source = default_source(Originators),
        networks = [network_to_v1(N) || N <- Networks],
        providers = [provider_to_v1(P) || P <- Providers],
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = MV,
        max_validity = MV,
        features = [feature_to_v1(F) || F <- Features]
    },

    ResponseDTO = #auth_resp_v1{
        req_id = ReqId,
        result = CustomerDTO
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO};
build_auth_response(<<"AuthRespV2">>, ReqId, Customer, UserId, Networks, Providers, Features) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        originators = Originators,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = _DV,
        max_validity = MV,
        pay_type = PayType,
        credit = Credit,
        credit_limit = CreditLimit,
        priority = Priority,
        rps = RPS
    } = Customer,

    CustomerDTO = #auth_customer_v2{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        user_id = UserId,
        pay_type = PayType,
        credit = Credit + CreditLimit,
        allowed_sources = allowed_sources(Originators),
        default_source = default_source(Originators),
        networks = [network_to_v1(N) || N <- Networks],
        providers = [provider_to_v1(P) || P <- Providers],
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = MV,
        max_validity = MV,
        features = [feature_to_v1(F) || F <- Features],
        priority = Priority,
        rps = RPS
    },

    ResponseDTO = #auth_resp_v2{
        req_id = ReqId,
        result = CustomerDTO
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO}.

build_error_response(<<"BindResponse">>, ReqId, {_, Reason}) ->
    ResponseDTO = #funnel_auth_response_dto{
        connection_id = ReqId,
        result = {error, atom_to_list(Reason)}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO};
build_error_response(<<"AuthRespV1">>, ReqId, {deny, Reason}) ->
    Response = #auth_resp_v1{
        req_id = ReqId,
        result = #auth_error_v1{
            code = Reason,
            message = "Request denied: " ++ atom_to_list(Reason)
        }
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response};
build_error_response(<<"AuthRespV1">>, ReqId, {error, Reason}) ->
    Response = #auth_resp_v1{
        req_id = ReqId,
        result = #auth_error_v1{
            code = Reason,
            message = "Request error: " ++ atom_to_list(Reason)
        }
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response};
build_error_response(<<"AuthRespV2">>, ReqId, {_, Reason}) ->
    Response = #auth_resp_v2{
        req_id = ReqId,
        result = #auth_error_v2{code = Reason}
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response}.

encode_response(ContentType, Response) ->
    case adto:encode(Response) of
        {ok, RespBin} ->
            {ContentType, RespBin};
        {error, Error} ->
            ?log_error("Unexpected auth response encode error: ~p", [Error]),
            noreply
    end.

allowed_sources(Originators) ->
    [O#originator.address || O <- Originators, O#originator.state =:= approved].

default_source(Originators) ->
    case [O#originator.address || O <- Originators,
            O#originator.state =:= approved, O#originator.is_default] of
        [] ->
            undefined;
        [Address | _] ->
            Address
    end.

network_to_dto(Network) ->
    #network{
        id = Id,
        country_code = CC,
        number_len = NL,
        prefixes = Pref,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = Network,
    #network_dto{
        id = Id,
        country_code = CC,
        %% now number_len in db without CC length or zero
        number_len = if NL =:= 0 -> 0; true -> NL + erlang:size(CC) end,
        prefixes = Pref,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

network_to_v1(Network) ->
    #network{
        id = Id,
        country_code = CC,
        number_len = NL,
        prefixes = Pref,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = Network,
    #network_v1{
        id = Id,
        country_code = CC,
        %% now number_len in db without CC length or zero
        number_len = if NL =:= 0 -> 0; true -> NL + erlang:size(CC) end,
        prefixes = Pref,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

provider_to_dto(Provider) ->
    #provider{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    } = Provider,
    #provider_dto{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    }.

provider_to_v1(Provider) ->
    #provider{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    } = Provider,
    #provider_v1{
        id = Id,
        gateway_id = GatewayId,
        bulk_gateway_id = BulkGatewayId,
        receipts_supported = RS,
        sms_add_points = SmsAddPoints
    }.

insert_if_not_member(P, Ps) ->
    case lists:member(P, Ps) of
        true -> Ps;
        false -> [P | Ps]
    end.

get_features(UserId, #customer{users = Users}) ->
    User = lists:keyfind(UserId, #user.id, Users),
    User#user.features.

feature_to_dto(Feature) ->
    #feature{
        name = Name,
        value = Value
    } = Feature,
    #feature_dto{
        name = Name,
        value = Value
    }.

feature_to_v1(Feature) ->
    #feature{
        name = Name,
        value = Value
    } = Feature,
    #feature_v1{
        name = Name,
        value = Value
    }.
