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
    case adto:decode(#funnel_auth_request_dto{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            CustomerId = AuthReq#funnel_auth_request_dto.customer_id,
            UserId     = AuthReq#funnel_auth_request_dto.user_id,
            Password   = AuthReq#funnel_auth_request_dto.password,
            ConnType   = AuthReq#funnel_auth_request_dto.type,
            ReqId      = AuthReq#funnel_auth_request_dto.connection_id,
            case authenticate(CustomerId, UserId, Password, ConnType) of
                {allow, Customer = #customer{}} ->
                    {ok, Networks, Providers} = build_networks_and_providers(Customer),
                    {ok, Response} = build_auth_response(<<"BindResponse">>,
                        ReqId, Customer, Networks, Providers),
                    ?log_debug("Auth allowed", []),
                    encode_response(<<"BindResponse">>, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(<<"BindResponse">>,
                        ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(<<"BindResponse">>, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(<<"BindResponse">>,
                        ReqId, {error, Reason}),
                    ?log_error("Auth error: ~p", [Reason]),
                    encode_response(<<"BindResponse">>, Response)
            end;
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            {ok, []}
    end;
process(<<"OneAPIAuthReq">>, ReqBin) ->
    case adto:decode(#k1api_auth_request_dto{}, ReqBin) of
        {ok, AuthReq} ->
            ?log_debug("Got auth request: ~p", [AuthReq]),
            CustomerId = AuthReq#k1api_auth_request_dto.customer_id,
            UserId     = AuthReq#k1api_auth_request_dto.user_id,
            Password   = AuthReq#k1api_auth_request_dto.password,
            ConnType   = AuthReq#k1api_auth_request_dto.connection_type,
            ReqId      = AuthReq#k1api_auth_request_dto.id,
            case authenticate(CustomerId, UserId, Password, ConnType) of
                {allow, Customer = #customer{}} ->
                    {ok, Networks, Providers} = build_networks_and_providers(Customer),
                    {ok, Response} = build_auth_response(<<"OneAPIAuthResp">>,
                        ReqId, Customer, Networks, Providers),
                    ?log_debug("Auth allowed", []),
                    encode_response(<<"OneAPIAuthResp">>, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(<<"OneAPIAuthResp">>,
                        ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(<<"OneAPIAuthResp">>, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(<<"OneAPIAuthResp">>,
                        ReqId, {error, Reason}),
                    ?log_error("Auth error: ~p", [Reason]),
                    encode_response(<<"OneAPIAuthResp">>, Response)
            end;
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            {ok, []}
    end;
process(ContentType, ReqBin) ->
    ?log_error("Got unknown auth request: ~p ~p", [ContentType, ReqBin]),
    {ok, []}.

-spec authenticate(customer_uuid(), user_id(), binary(), atom()) ->
    {allow, #customer{}} |
    {error, term()} |
    {deny, no_such_customer} |
    {deny, no_such_user} |
    {deny, password} |
    {deny, connection_type} |
    {deny, blocked} |
    {deny, deactivated} |
    {deny, credit_limit_exceeded}.
authenticate(CustomerId, UserId, Password, ConnType) ->
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case check_state(Customer#customer.state) of
                allow ->
                    case k_storage_customers:get_customer_user(Customer, UserId) of
                        {ok, User = #user{}} ->
                            ?log_debug("User found: ~p", [User]),
                            case check_password(Password, User#user.password) of
                                allow ->
                                    case check_state(User#user.state) of
                                        allow ->
                                            case check_conn_type(ConnType, User#user.connection_types) of
                                                allow ->
                                                    case check_credit_limit(Customer) of
                                                        allow ->
                                                            {allow, Customer};
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
                            {deny, no_such_user};
                        Error ->
                            ?log_error("Unexpected error: ~p", [Error]),
                            Error
                    end;
                Deny ->
                    Deny
            end;
        {error, no_entry} ->
            ?log_info("Customer id: ~p not found", [CustomerId]),
            {deny, no_such_customer};
        Error ->
            ?log_error("Unexpected error: ~p.", [Error]),
            Error
    end.

check_password(PasswHash, PasswHash) ->
    allow;
check_password(Passw, PasswHash) ->
    case ac_hexdump:binary_to_hexdump(crypto:hash(md5, Passw), to_lower) =:= PasswHash of
        true ->
            allow;
        _ ->
            {deny, password}
    end.

check_conn_type(ConnType, ConnTypes) ->
    case lists:member(ConnType, ConnTypes) of
        true ->
            allow;
        false ->
            {deny, connection_type}
    end.

check_state(active) ->
    allow;
check_state(blocked) ->
    {deny, blocked};
check_state(deactivated) ->
    {deny, deactivate}.

check_credit_limit(Customer) ->
    Credit = Customer#customer.credit,
    CreditLimit = Customer#customer.credit_limit,
    case Credit =< -CreditLimit of
        true ->
            {deny, credit_limit_exceeded};
        false ->
            allow
    end.

build_networks_and_providers(Customer) ->
    NetworkMapId = Customer#customer.network_map_id,
    DefaultProviderId = Customer#customer.default_provider_id,

    case k_storage_network_maps:get_network_map(NetworkMapId) of
        {ok, #network_map{network_ids = NetworkIds}} ->
            {NetworksDTO, ProvidersDTO} = lists:foldl(fun(NetworkId, {Ns, Ps})->
                case k_storage_networks:get_network(NetworkId) of
                    {ok, Network} ->
                        NetworkDTO = network_to_dto(Network),
                        ProviderId = Network#network.provider_id,
                        case k_storage_providers:get_provider(ProviderId) of
                            {ok, Provider} ->
                                ProviderDTO = provider_to_dto(Provider),
                                {[NetworkDTO | Ns], insert_if_not_member(ProviderDTO, Ps)};
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
            case DefaultProviderId of
                undefined ->
                    {ok, NetworksDTO, ProvidersDTO};
                _ ->
                    case k_storage_providers:get_provider(DefaultProviderId) of
                        {ok, DefaultProvider} ->
                            DefaultProviderDTO = provider_to_dto(DefaultProvider),
                            {ok, NetworksDTO, insert_if_not_member(DefaultProviderDTO, ProvidersDTO)};
                        {error, Error} ->
                            ?log_error("Get default provider id: ~p from customer id: ~p failed with: ~p",
                                [DefaultProviderId, Customer#customer.customer_uuid, Error]),
                            {ok, NetworksDTO, ProvidersDTO}
                    end
            end;
        {error, Error} ->
            ?log_error("Get map id: ~p failed with: ~p", [NetworkMapId, Error]),
            {ok, [], []}
    end.

build_auth_response(<<"BindResponse">>, ReqId, Customer, Networks, Providers) ->
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
        id = CustomerId,
        uuid = CustomerUuid,
        priority = Prio,
        rps = RPS,
        allowed_sources = allowed_sources(Originators),
        default_source = default_source(Originators),
        networks = Networks,
        providers = Providers,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = DV,
        max_validity = MV,
        pay_type = PayType
    },
    ResponseDTO = #funnel_auth_response_dto{
        connection_id = ReqId,
        result = {customer, CustomerDTO}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO};
build_auth_response(<<"OneAPIAuthResp">>, ReqId, Customer, Networks, Providers) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        priority = _Prio,
        originators = Originators,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = _DV,
        max_validity = MV,
        pay_type = PayType
    } = Customer,

    CustomerDTO = #k1api_auth_response_customer_dto{
        uuid = CustomerUuid,
        id = CustomerId,
        pay_type = PayType,
        allowed_sources = allowed_sources(Originators),
        default_source = default_source(Originators),
        networks = Networks,
        providers = Providers,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = MV,
        max_validity = MV
    },

    ResponseDTO = #k1api_auth_response_dto{
        id = ReqId,
        result = {customer, CustomerDTO}
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
build_error_response(<<"OneAPIAuthResp">>, ReqId, {deny, Reason}) ->
    Response = #k1api_auth_response_dto{
        id = ReqId,
        result = {error, "Request denied: " ++ atom_to_list(Reason)}
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response};
build_error_response(<<"OneAPIAuthResp">>, ReqId, {error, Reason}) ->
    Response = #k1api_auth_response_dto{
        id = ReqId,
        result = {error, "Request error: " ++ atom_to_list(Reason)}
    },

    ?log_debug("Built auth response: ~p", [Response]),
    {ok, Response}.

encode_response(ContentType, Response) ->
    case adto:encode(Response) of
        {ok, RespBin} ->
            {ContentType, RespBin};
        {error, Error} ->
            ?log_error("Unexpected auth response encode error: ~p", [Error]),
            {ok, []}
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

insert_if_not_member(P, Ps) ->
    case lists:member(P, Ps) of
        true -> Ps;
        false -> [P | Ps]
    end.
