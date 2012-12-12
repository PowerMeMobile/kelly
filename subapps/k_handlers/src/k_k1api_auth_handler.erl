-module(k_k1api_auth_handler).

-export([process/2]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/customer.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	?log_debug("Got k1api auth request", []),
	case adto:decode(#k1api_auth_request_dto{}, Message) of
		{ok, Request} ->
			process_auth_req(Request);
		{error, Error} ->
			?log_error("k1api auth request decode error: ~p", [Error]),
			?authentication_failed
	end.

process_auth_req(Request) ->
	case authenticate(Request) of
		{allow, Customer = #customer{}} ->
			{ok, ResponseDTO} = build_customer_response(Request, Customer),
			reply(ResponseDTO);
		{deny, Reason} ->
			?log_notice("k1api authentication denied: ~p", [Reason]),
			{ok, []};
		{error, Reason} ->
			?log_error("authentication error: ~p", [Reason]),
			{ok, []}
	end.

-spec authenticate(#k1api_auth_request_dto{}) ->
	{allow, #customer{}} |
	{error, term()} |
	{deny, no_such_customer} |
	{deny, password} |
	{deny, connection_type}.
authenticate(BindReq = #k1api_auth_request_dto{
	customer_id = SystemID,
	user_id = UserId }) ->
	?log_debug("Got k1api auth request: ~p", [BindReq]),

	case k_aaa:get_customer_by_system_id(SystemID) of
		{ok, Customer} ->
			?log_debug("Customer found: ~p", [Customer]),
			case k_aaa:get_customer_user(Customer, UserId) of
				{ok, User = #user{}} ->
					?log_debug("User found: ~p", [User]),
					Checks = [
						fun check_stage_password/2,
						fun check_stage_conntype/2
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

check_stage_password(#k1api_auth_request_dto{password = Pw}, #user{pswd_hash = PwHash}) ->
	case base64:encode(crypto:sha(Pw)) =:= PwHash of
		true ->
			allow;
		_ ->
			{deny, password}
	end.

check_stage_conntype(#k1api_auth_request_dto{}, #user{permitted_smpp_types = Types}) ->
	TypeRequested = oneapi,
	case lists:any(fun(T) -> T =:= TypeRequested end, Types) of
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

build_customer_response(#k1api_auth_request_dto{
	id = ConnectionId,
	customer_id = CustomerId
	}, #customer{
			uuid = UUID,
			billing_type = BillingType,
			allowed_sources = AllowedSources,
			default_source = DefaultSource,
			networks = NtwIdList,
			default_provider_id = DP,
			receipts_allowed = RA,
			no_retry = NR,
			default_validity = _DV,
			max_validity = MV }) ->

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

	CustomerDTO = #k1api_auth_response_dto{
		id = ConnectionId,
		system_id = CustomerId,
		uuid = UUID,
		billing_type = BillingType,
		allowed_sources = addr_to_dto(AllowedSources),
		default_source = addr_to_dto(DefaultSource),
		networks = Networks,
		providers = Providers,
		default_provider_id = DP,
		receipts_allowed = RA,
		no_retry = NR,
		default_validity = MV, %% fake
		max_validity = MV
	},
	?log_debug("Built customer: ~p", [CustomerDTO]),
	{ok, CustomerDTO}.

reply(Response) ->
	case adto:encode(Response) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.k1api.auth_response">>,
				content_type = <<"OneAPIAuthResponse">>,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api auth response encode error: ~p", [Error]),
	   		Error
	end.

addr_to_dto(undefined) ->
	undefined;
addr_to_dto(Addr = #addr{}) ->
	#addr{
		addr = Msisdn,
		ton = TON,
		npi = NPI
	} = Addr,
	#addr_dto{
		addr = Msisdn,
		ton = TON,
		npi = NPI
	};
addr_to_dto(Addrs) ->
	[addr_to_dto(Addr) || Addr <- Addrs].
