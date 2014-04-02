-ifndef(customer_hrl).
-define(customer_hrl, included).

-include("storages.hrl").
-include("network.hrl").
-include("provider.hrl").

-type customer_uuid() :: uuid().
-type customer_id() :: binary(). %% http customer_id | smpp system-type
-type user_id() :: binary(). %% http user_id | smpp system-id
-type bind_type() :: transmitter | receiver | tranceiver | oneapi | soap | mm.
-type pay_type() :: prepaid | postpaid.

-record(user, {
	id			:: user_id(),
	password	:: binary(),
	bind_types	:: [bind_type()]
}).
-type user() :: #user{}.

-record(customer, {
	customer_uuid		:: customer_uuid(),
	customer_id			:: customer_id(),
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
	pay_type	    	:: pay_type(),
	state = 0			:: non_neg_integer() %% 0 blocked, 1 active
}).
-type customer() 		:: #customer{}.

-endif.
