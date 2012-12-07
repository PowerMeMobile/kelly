-module(k_aaa_customers).

-define(CURRENT_VERSION, 1).

%% API
-export([
	get_customers/0,
	get_customer/1,
	set_customer/2,
	del_customer/1
]).

-include_lib("k_common/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_customer(customer_id(), #customer{}) -> ok | {error, term()}.
set_customer(CustomerId, Customer) ->
	kv_storage_common:write_version(customers, ?CURRENT_VERSION, CustomerId, Customer).

-spec get_customers() -> {ok, [{customer_id(), #customer{}}]} | {error, term()}.
get_customers() ->
	kv_storage_common:read_version(customers, ?CURRENT_VERSION).

-spec get_customer(customer_id()) -> {ok, #customer{}} | {error, no_entry} | {error, term()}.
get_customer(CustomerId) ->
	kv_storage_common:read_version(customers, ?CURRENT_VERSION, CustomerId).

-spec del_customer(customer_id()) -> ok | {error, no_entry} | {error, term()}.
del_customer(CustomerId) ->
	kv_storage_common:delete_version(customers, ?CURRENT_VERSION, CustomerId).
