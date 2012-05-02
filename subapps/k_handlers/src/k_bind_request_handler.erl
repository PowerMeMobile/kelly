-module(k_bind_request_handler).

-export([process/2]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/FunnelAsn.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	% ?log_debug("got message...", []),
	case 'FunnelAsn':decode('BindRequest', Message) of
		{ok, Request = #'BindRequest'{}} ->
			case authenticate(Request) of
				{allow, Customer = #customer{}} ->
					build_response(Request, Customer);
				Error ->
					?log_error("authentication error: ~p", [Error]),
					?authentication_failed
			end;
		Error ->
			?log_error("asn decode error: ~p", [Error]),
			?authentication_failed
	end.

-spec authenticate(#'BindRequest'{}) ->
	{allow, #customer{}} |
	{error, term()} |
	{deny, no_such_customer} |
	{deny, password} |
	{deny, connection_type}.
authenticate(BindReq = #'BindRequest'{
	customerId = SystemId,
	userId = UserId
}) ->
	?log_debug("got request: ~p", [BindReq]),
	case k_aaa_api:get_customer_by_system_id(SystemId) of
		{ok, Customer} ->
			case k_aaa_api:get_customer_user(Customer, UserId) of
				{ok, User = #user{}} ->
					Checks = [
						fun check_stage_password/2,
						fun check_stage_conntype/2
					],
					perform_checks(BindReq, User, Checks, Customer);
				{error, no_record} ->
					{deny, no_such_customer};
				Other ->
					Other
			end;
		Any ->
			Any
	end.

check_stage_password(#'BindRequest'{password = Pw}, #user{pswd_hash = PwHash}) ->
	case crypto:sha(Pw) =:= PwHash of
		true ->
			allow;
		_ ->
			{deny, password}
	end.

check_stage_conntype(#'BindRequest'{type = TypeRequested}, #user{permitted_smpp_types = Types}) ->
	case lists:any(fun(T) -> T =:= TypeRequested end, Types) of
		true ->
			allow;
		_ ->
			{deny, connection_type}
	end.

perform_checks(_, _, [], Customer) -> {allow, Customer};
perform_checks(BindReq = #'BindRequest'{}, User = #user{}, [Check | SoFar], Customer) ->
	case Check(BindReq, User) of
		allow ->
			perform_checks(BindReq, User, SoFar, Customer);
		{deny, DenyReason} ->
			{deny, DenyReason}
	end.

build_response(#'BindRequest'{
	connectionId = ConnectionId,
	customerId = CustomerId
	}, #customer{
			uuid = UUID,
			priority = Prior,
			rps = RPSt,
			allowedSources = AddrList,
			defaultSource = DSt,
			networks = NtwIdList,
			defaultProviderId = DPt,
			receiptsAllowed = RA,
			noRetry = NR,
			defaultValidity = DV,
			maxValidity = MV
	}) ->
	% ?log_debug("building bind response...", []),

	%%% validation optional values %%%
	RPS = validate_optional_asn_value(RPSt),
	DS = validate_optional_asn_value(DSt),
	DP = validate_optional_asn_value(DPt),
	%%% END of validation %%%

	Addrs = lists:map(fun(#addr{
								addr = Addr,
								ton = TON,
								npi = NPI })->
		#'Addr'{
			addr = Addr,
			ton = TON,
			npi = NPI
		}

	end, AddrList),

	% ?log_debug("built Addr: ~p", [Addrs]),

	{Networks, Providers} = lists:foldl(fun(NetworkId, {N, P})->
		%%%NETWORK SECTION%%%
		{ok, Network} = k_config_api:get_network(NetworkId),
		#network{
			countryCode = CC,
			numbersLen = NL,
			prefixes = Pref,
			providerId = ProviderId
			} = Network,
		NNew = #'Network'{
			id = NetworkId,
			countryCode = CC,
			numbersLen = NL,
			prefixes = Pref,
			providerId = ProviderId
		},

		%%% PROVIDER SECTION %%%
		{ok, Provider} = k_config_api:get_provider(ProviderId),
		#provider{
			gateway = Gateway,
			bulkGateway = BGateway,
			receiptsSupported = RS
		} = Provider,
		PNew = #'Provider'{
			id = ProviderId,
			gateway = Gateway,
			bulkGateway = BGateway,
			receiptsSupported = RS
		},
		%%% check for Provider dublications %%%
		case lists:member(PNew, P) of
			true ->
				{[NNew | N], P};
			false ->
				{[NNew | N], [PNew | P]}
		end
	end, {[], []}, NtwIdList),
	% ?log_debug("built Networks: ~p", [Networks]),
	% ?log_debug("built Providers: ~p", [Providers]),

	Customer = #'Customer'{
		id = CustomerId,
		uuid = UUID,
		priority = Prior,
		rps = RPS,
		allowedSources = Addrs,
		defaultSource = DS,
		networks = Networks,
		providers = Providers,
		defaultProviderId = DP,
		receiptsAllowed = RA,
		noRetry = NR,
		defaultValidity = DV,
		maxValidity = MV
	},
	?log_debug("built Customer: ~p", [Customer]),

	Response = #'BindResponse'{
		connectionId = ConnectionId,
		result = {customer, Customer}
	},
	case 'FunnelAsn':encode('BindResponse', Response) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.funnel.server_control">>,
				content_type = <<"BindResponse">>,
				payload = Binary},
			% ?log_debug("response: ~p", [Reply]),
			{ok, [Reply]};
		Error ->
	   		Error
	end.

validate_optional_asn_value(OptionValue) ->
	case OptionValue of
			undefined ->
				asn1_NOVALUE;
			Value ->
				Value
	end.
