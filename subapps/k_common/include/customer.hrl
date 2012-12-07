-ifndef(customer_hrl).
-define(customer_hrl, included).

-include("storages.hrl").
-include("network.hrl").
-include("provider.hrl").

-type smpp_connection_type() :: transmitter | receiver | tranceiver | oneapi.
-type billing_type() 	:: prepaid | postpaid.
-type user_id() 		:: binary().

-record(user, {
	id 					:: user_id(),
	pswd_hash 			:: binary(),
	permitted_smpp_types :: [smpp_connection_type()]
}).
-type user() :: #user{}.

-record(customer, {
	id 					:: system_id(),
	uuid 				:: customer_id(),
	name 				:: binary(),
	priority 			:: integer(),
	rps 				:: integer() | undefined,
	allowed_sources		:: [addr()], %% originators
	default_source 		:: addr() | undefined, %% default originator
	networks 			:: [network_id()],
	default_provider_id	:: provider_id() | undefined,
	receipts_allowed 	:: boolean(),
	no_retry 			:: boolean(),
	default_validity 	:: binary(),
	max_validity		:: integer(),
	users = []			:: [user()] | [],
	billing_type		:: billing_type(),
	state = 0			:: non_neg_integer() %% 0 blocked, 1 active
}).
-type customer_id() 	:: uuid().
-type system_id() 		:: binary().
-type customer() 		:: {ver(), #customer{} }.

-endif.
