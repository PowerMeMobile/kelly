-module(k_bind_request_handler).

-export([process/2]).

-define(authentication_failed, {ok, []}).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/customer.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#funnel_auth_request_dto{}, Message) of
		{ok, Request} ->
			Response =
				case authenticate(Request) of
					{allow, Customer = #customer{}} ->
						build_customer_response(Request, Customer);
					{deny, Reason} ->
						?log_notice("funnel authentication denied: ~p", [Reason]),
						build_error_response(Request, Reason);
					{error, Reason} ->
						?log_error("authentication error: ~p", [Reason]),
						build_error_response(Request, Reason)
				end,
			reply(Response);
		{error, Error} ->
			?log_error("Funnel auth request decode error: ~p", [Error]),
			?authentication_failed
	end.

-spec authenticate(#funnel_auth_request_dto{}) ->
	{allow, #customer{}} |
	{error, term()} |
	{deny, no_such_customer} |
	{deny, password} |
	{deny, connection_type}.
authenticate(BindReq = #funnel_auth_request_dto{
	customer_id = SystemID,
	user_id = UserId
}) ->
	?log_debug("Got funnel auth request: ~p", [BindReq]),

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

check_stage_password(#funnel_auth_request_dto{password = Pw}, #user{pswd_hash = PwHash}) ->
	case base64:encode(crypto:sha(Pw)) =:= PwHash of
		true ->
			allow;
		_ ->
			{deny, password}
	end.

check_stage_conntype(#funnel_auth_request_dto{type = TypeRequested}, #user{permitted_smpp_types = Types}) ->
	case lists:any(fun(T) -> T =:= TypeRequested end, Types) of
		true ->
			allow;
		_ ->
			{deny, connection_type}
	end.

perform_checks(_, _, [], Customer) ->
	?log_debug("Funnel auth allowed", []),
	{allow, Customer};
perform_checks(BindReq, User = #user{}, [Check | SoFar], Customer) ->
	case Check(BindReq, User) of
		allow ->
			perform_checks(BindReq, User, SoFar, Customer);
		{deny, DenyReason} ->
			{deny, DenyReason}
	end.

build_customer_response(#funnel_auth_request_dto{
	connection_id = ConnectionId,
	customer_id = CustomerId
	}, #customer{
			uuid = UUID,
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
			billing_type = BillingType
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
		uuid = UUID,
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
		billing_type = BillingType
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
	case adto:encode(Response) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.funnel.server_control">>,
				content_type = <<"BindResponse">>,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected funnel auth response encode error: ~p", [Error]),
	   		Error
	end.
