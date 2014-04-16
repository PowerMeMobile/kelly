-module(k_bind_request_handler).

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
    case adto:decode(#funnel_auth_request_dto{}, Payload) of
        {ok, AuthReq} ->
            process_auth_req(Req, AuthReq);
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            ?authentication_failed
    end.

%% ===================================================================
%% Internal
%% ===================================================================

process_auth_req(Req, AuthReq) ->
    case authenticate(AuthReq) of
        {allow, Customer = #customer{}} ->
            {ok, Response} = build_customer_response(AuthReq, Customer),
            reply(Req, Response);
        {deny, Reason} ->
            {ok, Response} = build_error_response(AuthReq, Reason),
            ?log_notice("Auth denied: ~p", [Reason]),
            reply(Req, Response);
        {error, Reason} ->
            {ok, Response} = build_error_response(AuthReq, Reason),
            ?log_error("Auth error: ~p", [Reason]),
            reply(Req, Response)
    end.

-spec authenticate(#funnel_auth_request_dto{}) ->
    {allow, #customer{}} |
    {error, term()} |
    {deny, no_such_customer} |
    {deny, password} |
    {deny, connection_type}.
authenticate(BindReq = #funnel_auth_request_dto{
    customer_id = CustomerId,
    user_id = UserId
}) ->
    ?log_debug("Got auth request: ~p", [BindReq]),
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            ?log_debug("Customer found: ~p", [Customer]),
            case k_storage_customers:get_customer_user(Customer, UserId) of
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

check_password(
    #funnel_auth_request_dto{password = Pw}, #user{password = PwHash}
) ->
    case ac_hexdump:binary_to_hexdump(crypto:hash(md5, Pw), to_lower) =:= PwHash of
        true ->
            allow;
        _ ->
            {deny, password}
    end.

check_bind_type(BindReq, User) ->
    BindType = BindReq#funnel_auth_request_dto.type,
    ConnTypes = User#user.connection_types,
    case lists:member(BindType, ConnTypes) of
        true ->
            allow;
        false ->
            {deny, connection_type}
    end.

perform_checks(_, _, [], Customer) ->
    ?log_debug("Auth allowed", []),
    {allow, Customer};
perform_checks(BindReq, User = #user{}, [Check | SoFar], Customer) ->
    case Check(BindReq, User) of
        allow ->
            perform_checks(BindReq, User, SoFar, Customer);
        {deny, DenyReason} ->
            {deny, DenyReason}
    end.

build_customer_response(Request, Customer) ->
    #funnel_auth_request_dto{
        connection_id = ConnectionId
    } = Request,

    #customer{
        customer_uuid = CustomerUuid,
        customer_id = CustomerId,
        priority = Prio,
        rps = RPS,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
        network_map_id = NetworkMapId,
        default_provider_id = DP,
        receipts_allowed = RA,
        no_retry = NR,
        default_validity = DV,
        max_validity = MV,
        pay_type = PayType
    } = Customer,

    {ok, #network_map{network_ids = NetworkIds}} =
        k_storage_network_maps:get_network_map(NetworkMapId),

    {Networks, Providers} = lists:foldl(fun(NetworkId, {Ns, Ps})->
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

        %% check for Providers dublications
        case lists:member(ProviderDTO, Ps) of
            true ->
                {[NetworkDTO | Ns], Ps};
            false ->
                {[NetworkDTO | Ns], [ProviderDTO | Ps]}
        end
    end, {[], []}, NetworkIds),

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
        connection_id = ConnectionId,
        result = {customer, CustomerDTO}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO}.

build_error_response(#funnel_auth_request_dto{connection_id = ConnectionId}, Reason) ->
    ResponseDTO = #funnel_auth_response_dto{
        connection_id = ConnectionId,
        result = {error, atom_to_list(Reason)}
    },
    ?log_debug("Built auth response: ~p", [ResponseDTO]),
    {ok, ResponseDTO}.

reply(Req, Response) ->
    {ok, ReplyTo} = k_amqp_req:reply_to(Req),
    case adto:encode(Response) of
        {ok, Binary} ->
            Reply = #worker_reply{
                reply_to = ReplyTo,
                content_type = <<"BindResponse">>,
                payload = Binary
            },
            {ok, [Reply]};
        Error ->
            ?log_warn("Unexpected auth response encode error: ~p", [Error]),
            Error
    end.
