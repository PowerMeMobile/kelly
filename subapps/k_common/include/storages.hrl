-ifndef(storages_hrl).
-define(storages_hrl, included).

-type ver() :: integer().
-type error() :: term().
-type uuid() :: string().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gateway storage
% {
% 	Key :: gateway_id(),
% 	Val :: gateway()
% }
-type connection_id() :: integer().
-record(connection, {
	id 			:: connection_id(),
	type 		:: integer(),
	addr 		:: string(),
	port 		:: integer(),
	sys_id 		:: string(),
	pass 		:: string(),
	sys_type 	:: string(),
	addr_ton 	:: integer(),
	addr_npi 	:: integer(),
	addr_range 	:: string()
}).
-type connection() :: #connection{}.
-record(gateway, {
	name 			 :: string(),
	rps 			 :: integer(),
	connections = [] :: [connection()] | []
	%%% Here new fields will be added for ever new gateway's setting
}).
-type gateway_id() :: uuid().
-type gateway() :: {ver(), #gateway{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% provider storage
% {
% 	Key :: provider_id(),
% 	Val :: provider()
% }
-record(provider, {
	gateway 		  :: gateway_id(),
	bulkGateway 	  :: gateway_id(),
	receiptsSupported :: boolean()
}).
-type provider_id() :: string().
-type provider() :: {ver(), #provider{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% network storage
% {
% 	Key :: network_id(),
% 	Val :: network()
% }
-record(network, {
	countryCode :: string(),
	numbersLen :: integer(),
	prefixes :: [string()],
	providerId :: provider_id()
}).
-type network_id() :: string().
-type network() :: {ver(), #network{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% customer storage
% {
% 	Key :: customer_id()
% 	Val :: customer()
% }

-record(addr, {
	addr :: string(),
	ton :: integer(),
	npi :: integer()
}).
-type addr() :: #addr{}.

-type smpp_connection_type() :: transmitter | receiver | tranceiver.
-type user_id() :: string().
-record(user, {
	id :: user_id(),
	pswd_hash :: binary(),
	permitted_smpp_types :: [smpp_connection_type()]
}).
-type user() :: #user{}.

-record(customer, {
	id 					:: customer_id(),
	uuid 				:: uuid(),
	name 				:: string(),
	priority 			:: integer(),
	rps 				:: integer() | undefined,
	allowedSources 		:: [addr()], %% originators
	defaultSource 		:: addr() | undefined, %% default originator
	networks 			:: [network_id()],
	defaultProviderId	:: provider_id() | undefined,
	receiptsAllowed 	:: boolean(),
	noRetry 			:: boolean(),
	defaultValidity 	:: string(),
	maxValidity			:: integer(),
	users = []			:: [user()] | [],
	state = 0			:: non_neg_integer() %% 0 blocked, 1 active
}).
-type customer_id() :: uuid().
-type system_id() :: uuid().
-type customer() :: {ver(), #customer{} }.


-endif. % storages_hrl
