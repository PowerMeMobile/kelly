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
%% deprecated since funnel 2.11.0
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
                    {ok, Response} =
                        build_auth_response(RespCT, ReqId, Customer, UserId),
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
                    {ok, Response} =
                        build_auth_response(RespCT, ReqId, Customer, User#user.id),
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
process(<<"AuthReqV3">>, ReqBin) ->
    RespCT = <<"AuthRespV3">>,
    case adto:decode(#auth_req_v3{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            AuthData  = AuthReq#auth_req_v3.auth_data,
            Interface = AuthReq#auth_req_v3.interface,
            ReqId     = AuthReq#auth_req_v3.req_id,
            case authenticate_by(AuthData, Interface) of
                {allow, Customer = #customer{}, User = #user{}} ->
                    {ok, Response} =
                        build_auth_response(RespCT, ReqId, Customer, User#user.id),
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

% deprecated since funnel 2.11.0
build_auth_response(<<"BindResponse">>, ReqId, Customer, UserId) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        priority = Prio,
        rps = RPS,
        originators = Originators,
        network_map_id = NetMapId,
        default_provider_id = DefProvId,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = DV,
        max_validity = MV,
        pay_type = PayType
    } = Customer,

    {ok, Networks, Providers} =
        k_handlers_utils:get_networks_and_providers(NetMapId, DefProvId),

    Features = get_features(UserId, Customer),

    CustomerDTO = #funnel_auth_response_customer_dto{
        uuid = CustomerUuid,
        id = CustomerId,
        priority = Prio,
        rps = RPS,
        allowed_sources = allowed_originators(Originators),
        default_source = default_originator(Originators),
        networks = [network_to_dto(N) || N <- Networks],
        providers = [provider_to_dto(P) || P <- Providers],
        default_provider_id = DefProvId,
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
build_auth_response(<<"AuthRespV2">>, ReqId, Customer, UserId) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        originators = Originators,
        network_map_id = NetMapId,
        default_provider_id = DefProvId,
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

    {ok, Networks, Providers} =
        k_handlers_utils:get_networks_and_providers(NetMapId, DefProvId),

    Features = get_features(UserId, Customer),

    CustomerDTO = #auth_customer_v2{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        user_id = UserId,
        pay_type = PayType,
        credit = Credit + CreditLimit,
        allowed_sources = allowed_originators(Originators),
        default_source = default_originator(Originators),
        networks = [network_to_v1(N) || N <- Networks],
        providers = [k_handlers_utils:provider_to_v1(P) || P <- Providers],
        default_provider_id = DefProvId,
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
    {ok, ResponseDTO};
build_auth_response(<<"AuthRespV3">>, ReqId, Customer, UserId) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        originators = Originators,
        network_map_id = NetMapId,
        default_provider_id = DefProvId,
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

    {ok, CustNs, CustPs} =
        k_handlers_utils:get_networks_and_providers(NetMapId, DefProvId),

    CustCoverage = build_auth_coverage_v1(customer, CustNs, CustPs),

    Origs = [O || O <- Originators, O#originator.state =:= approved,
                                    O#originator.routings =/= []],

    OrigCoverages =
        [build_originator_auth_coverage_v1(CustNs, CustPs, O) || O <- Origs],

    Coverages = [CustCoverage | OrigCoverages],

    Features = get_features(UserId, Customer),

    CustomerDTO = #auth_customer_v3{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        user_id = UserId,
        pay_type = PayType,
        credits = Credit + CreditLimit,
        originators = allowed_originators(Originators),
        default_originator = default_originator(Originators),
        coverages = Coverages,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = MV,
        max_validity = MV,
        features = [feature_to_v1(F) || F <- Features],
        priority = Priority,
        rps = RPS
    },

    ResponseDTO = #auth_resp_v3{
        req_id = ReqId,
        result = CustomerDTO
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO}.

% deprecated since funnel 2.11.0
build_error_response(<<"BindResponse">>, ReqId, {_, Reason}) ->
    ResponseDTO = #funnel_auth_response_dto{
        connection_id = ReqId,
        result = {error, atom_to_list(Reason)}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO};
build_error_response(<<"AuthRespV2">>, ReqId, {_, Reason}) ->
    Response = #auth_resp_v2{
        req_id = ReqId,
        result = #auth_error_v2{code = Reason}
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response};
build_error_response(<<"AuthRespV3">>, ReqId, {_, Reason}) ->
    Response = #auth_resp_v3{
        req_id = ReqId,
        result = #auth_error_v3{code = Reason}
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

allowed_originators(Originators) ->
    [O#originator.address || O <- Originators, O#originator.state =:= approved].

default_originator(Originators) ->
    case [O#originator.address || O <- Originators,
            O#originator.state =:= approved, O#originator.is_default] of
        [] ->
            undefined;
        [Address | _] ->
            Address
    end.

build_originator_auth_coverage_v1(CustNs, CustPs, Orig) ->
    Addr = Orig#originator.address,
    Routings = Orig#originator.routings,
    {OrigNs, OrigPs} = lists:foldl(
        fun(R, {Ns, Ps}) -> merge_routing(Ns, Ps, R) end,
        {CustNs, CustPs},
        Routings
    ),
    build_auth_coverage_v1(Addr, OrigNs, OrigPs).

merge_routing(Ns, Ps, R) ->
    NetMapId  = R#routing.network_map_id,
    DefProvId = R#routing.default_provider_id,
    {ok, OrigNs, OrigPs} =
        k_handlers_utils:get_networks_and_providers(NetMapId, DefProvId),
    merge_networks_providers(Ns, Ps, OrigNs, OrigPs).

merge_networks_providers(Ns, Ps, OrigNs, OrigPs) ->
    OrigNs2 = lists:foldl(
        fun(OrigNet, Acc) ->
            lists:keyreplace(OrigNet#network.id, #network.id, Acc, OrigNet)
        end,
        Ns,
        OrigNs
    ),
    {OrigNs2, OrigPs ++ Ps}.

build_auth_coverage_v1(Id, Networks, Providers) ->
    #auth_coverage_v1{
        id = Id,
        networks = [k_handlers_utils:network_to_v1(N) || N <- Networks],
        providers = [k_handlers_utils:provider_to_v1(P) || P <- Providers]
    }.

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

%% Note it's different from k_handlers_utils:network_to_v1/1
%% this one possibly adds CC length.
network_to_v1(N) ->
    #network{
        id = Id,
        name = Name,
        country_code = CC,
        number_len = NL,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    } = N,
    #network_v1{
        id = Id,
        name = Name,
        country_code = CC,
        %% now number_len in db without CC length or zero
        number_len = if NL =:= 0 -> 0; true -> NL + erlang:size(CC) end,
        prefixes = Prefixes,
        provider_id = ProviderId,
        is_home = IsHome,
        country = Country,
        gmt_diff = GMTDiff,
        dst = DST,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    }.

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
