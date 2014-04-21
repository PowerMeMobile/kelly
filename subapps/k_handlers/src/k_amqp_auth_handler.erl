-module(k_amqp_auth_handler).

-export([start_link/0]).
-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
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
                    {ok, Networks, Providers} = build_networks_providers(Customer),
                    {ok, Response} = build_auth_response(<<"BindResponse">>, ReqId, Customer, Networks, Providers),
                    ?log_debug("Auth allowed", []),
                    encode_response(<<"BindResponse">>, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(<<"BindResponse">>, ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(<<"BindResponse">>, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(<<"BindResponse">>, ReqId, {error, Reason}),
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
                    {ok, Networks, Providers} = build_networks_providers(Customer),
                    {ok, Response} = build_auth_response(<<"OneAPIAuthResp">>, ReqId, Customer, Networks, Providers),
                    ?log_debug("Auth allowed", []),
                    encode_response(<<"OneAPIAuthResp">>, Response);
                {deny, Reason} ->
                    {ok, Response} = build_error_response(<<"OneAPIAuthResp">>, ReqId, {deny, Reason}),
                    ?log_notice("Auth denied: ~p", [Reason]),
                    encode_response(<<"OneAPIAuthResp">>, Response);
                {error, Reason} ->
                    {ok, Response} = build_error_response(<<"OneAPIAuthResp">>, ReqId, {error, Reason}),
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
    {deny, password} |
    {deny, connection_type} |
    {deny, blocked}.
authenticate(CustomerId, UserId, Password, ConnType) ->
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case k_storage_customers:get_customer_user(Customer, UserId) of
                {ok, User = #user{}} ->
                    ?log_debug("User found: ~p", [User]),
                    case check_password(Password, User#user.password) of
                        allow ->
                            case check_conn_type(ConnType, User#user.connection_types) of
                                allow ->
                                    case check_blocked(Customer#customer.state) of
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

check_blocked(active) ->
    allow;
check_blocked(blocked) ->
    {deny, blocked}.

build_networks_providers(Customer) ->
    NetworkMapId = Customer#customer.network_map_id,

    {ok, #network_map{network_ids = NetworkIds}} =
        k_storage_network_maps:get_network_map(NetworkMapId),

    {NetworksDTO, ProvidersDTO} = lists:foldl(fun(NetworkId, {Ns, Ps})->
        {ok, Network} = k_storage_networks:get_network(NetworkId),
        #network{
            country_code = CC,
            number_len = NL,
            prefixes = Pref,
            provider_id = ProviderId
        } = Network,
        NetworkDTO = #network_dto{
            id = NetworkId,
            country_code = CC,
            %% now number_len in db without CC length
            number_len = NL + erlang:size(CC),
            prefixes = Pref,
            provider_id = ProviderId
        },

        {ok, Provider} = k_storage_providers:get_provider(ProviderId),
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

        %% check for Providers duplications.
        case lists:member(ProviderDTO, Ps) of
            true ->
                {[NetworkDTO | Ns], Ps};
            false ->
                {[NetworkDTO | Ns], [ProviderDTO | Ps]}
        end
    end, {[], []}, NetworkIds),

    {ok, NetworksDTO, ProvidersDTO}.

build_auth_response(<<"BindResponse">>, ReqId, Customer, Networks, Providers) ->
    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        priority = Prio,
        rps = RPS,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
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
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
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
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
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
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
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
