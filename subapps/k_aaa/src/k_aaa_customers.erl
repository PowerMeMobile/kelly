-module(k_aaa_customers).

-define(CURRENT_VERSION, 1).

%% API
-export([
	get_customer/1,
	set_customer/2,
	del_customer/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_id(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerId, Customer) ->
	k_gen_storage_common:write(customers, ?CURRENT_VERSION, CustomerId, Customer).

-spec get_customer(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer(CustomerId) ->
	k_gen_storage_common:read(customers, ?CURRENT_VERSION, CustomerId).

-spec del_customer(customer_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerId) ->
	k_gen_storage_common:delete(customers, ?CURRENT_VERSION, CustomerId).
