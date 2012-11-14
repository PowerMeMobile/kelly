-ifndef(storages_hrl).
-define(storages_hrl, included).

-type ver() 			:: integer().
-type error() 			:: term().
-type uuid() 			:: binary().

%% ===================================================================
%% Gateway Storage
%% ===================================================================

% {
% 	Key :: gateway_id(),
% 	Val :: gateway()
% }

-type connection_id() 	:: integer().
-record(connection, {
	id 					:: connection_id(),
	type 				:: integer(),
	addr 				:: bitstring(),
	port 				:: integer(),
	sys_id 				:: bitstring(),
	pass 				:: bitstring(),
	sys_type 			:: bitstring(),
	addr_ton 			:: integer(),
	addr_npi 			:: integer(),
	addr_range 			:: bitstring()
}).
-type connection() 		:: #connection{}.
-record(gateway, {
	name 			 	:: bitstring(),
	rps 			 	:: integer(),
	connections = [] 	:: [connection()] | []
	%%% Here new fields will be added for ever new gateway's setting
}).
-type gateway_id() 		:: uuid().
-type gateway() 		:: {ver(), #gateway{}}.

%% ===================================================================
%% Provider Storage
%% ===================================================================

% provider storage
% {
% 	Key :: provider_id(),
% 	Val :: provider()
% }

-record(provider, {
	name 			  	:: bitstring(),
	gateway 		  	:: gateway_id(),
	bulkGateway 	  	:: gateway_id(),
	receiptsSupported 	:: boolean()
}).
-type provider_id() 	:: uuid().
-type provider() 		:: {ver(), #provider{}}.

%% ===================================================================
%% Network Storage
%% ===================================================================

% network storage
% {
% 	Key :: network_id(),
% 	Val :: network()
% }

-record(network, {
	name 				:: bitstring(),
	countryCode 		:: bitstring(),
	numbersLen 			:: integer(),
	prefixes 			:: [bitstring()],
	providerId 			:: provider_id()
}).
-type network_id() 		:: uuid().
-type network() 		:: {ver(), #network{}}.

%% ===================================================================
%% Customer Storage
%% ===================================================================

% customer storage
% {
% 	Key :: customer_id()
% 	Val :: customer()
% }

-record(addr, {
	addr 				:: bitstring(),
	ton 				:: integer(),
	npi 				:: integer()
}).
-type addr() 			:: #addr{}.

-type smpp_connection_type() :: transmitter | receiver | tranceiver | oneapi.
-type billing_type() 	:: prepaid | postpaid.
-type user_id() 		:: bitstring().
-record(user, {
	id 					:: user_id(),
	pswd_hash 			:: binary(),
	permitted_smpp_types :: [smpp_connection_type()]
}).
-type user() :: #user{}.

-record(customer, {
	id 					:: system_id(),
	uuid 				:: customer_id(),
	name 				:: bitstring(),
	priority 			:: integer(),
	rps 				:: integer() | undefined,
	allowedSources 		:: [addr()], %% originators
	defaultSource 		:: addr() | undefined, %% default originator
	networks 			:: [network_id()],
	defaultProviderId	:: provider_id() | undefined,
	receiptsAllowed 	:: boolean(),
	noRetry 			:: boolean(),
	defaultValidity 	:: bitstring(),
	maxValidity			:: integer(),
	users = []			:: [user()] | [],
	billing_type		:: billing_type(),
	state = 0			:: non_neg_integer() %% 0 blocked, 1 active
}).
-type customer_id() 	:: uuid().
-type system_id() 		:: bitstring().
-type customer() 		:: {ver(), #customer{} }.

-endif. % storages_hrl
