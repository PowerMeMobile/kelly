-module(k_bind_request_handler).

-export([process/1]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

-spec process(k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, Payload} = k_amqp_req:payload(Req),
    case adto:decode(#funnel_auth_request_dto{}, Payload) of
        {ok, Request} ->
            Response =
                case authenticate(Request) of
                    {allow, Customer = #customer{}} ->
                        build_customer_response(Request, Customer);
                    {deny, Reason} ->
                        ?log_notice("Auth denied: ~p", [Reason]),
                        build_error_response(Request, Reason);
                    {error, Reason} ->
                        ?log_error("Auth error: ~p", [Reason]),
                        build_error_response(Request, Reason)
                end,
            reply(Response);
        {error, Error} ->
            ?log_error("Auth request decode error: ~p", [Error]),
            ?authentication_failed
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

build_customer_response(#funnel_auth_request_dto{
    connection_id = ConnectionId
}, #customer{
            customer_uuid = CustomerUUID,
            customer_id = CustomerId,
            priority = Prior,
            rps = RPS,
            allowed_sources = AllowedSources,
            default_source = DefaultSource,
            networks = NtwIdList,
            default_provider_id = DP,
            receipts_allowed = RA,
            no_retry = NR,
            default_validity = DV,
            max_validity = MV,
            pay_type = PayType
     }) ->

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

    Customer = #funnel_auth_response_customer_dto{
        id = CustomerId,
        uuid = CustomerUUID,
        priority = Prior,
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
    ?log_debug("Built customer: ~p", [Customer]),
    #funnel_auth_response_dto{
        connection_id = ConnectionId,
        result = {customer, Customer}
    }.

build_error_response(#funnel_auth_request_dto{connection_id = ConnectionId}, Reason) ->
    ?log_debug("Building auth error response...", []),
    #funnel_auth_response_dto{
        connection_id = ConnectionId,
        result = {error, atom_to_list(Reason)}
    }.

reply(Response) ->
    {ok, ReplyTo} = application:get_env(k_handlers, funnel_control_queue),
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
