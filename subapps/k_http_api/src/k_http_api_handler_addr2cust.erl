-module(k_http_api_handler_addr2cust).

-behaviour(gen_cowboy_crud).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include("crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_mailbox/include/address.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
	Read = #method_spec{
				path = [<<"addr2cust">>, msisdn],
				params = [#param{name = msisdn, mandatory = true, repeated = false, type = addr}]},

	DeleteParams = [
		#param{name = msisdn, mandatory = true, repeated = false, type = addr}
	],
	Delete = #method_spec{
				path = [<<"addr2cust">>, msisdn],
				params = DeleteParams},

	CreateParams = [
		#param{name = msisdn, mandatory = true, repeated = false, type = addr},
		#param{name = customer,	mandatory = true, repeated = false,	type = binary_uuid},
		#param{name = user, mandatory = true, repeated = false, type = binary}
	],
	Create = #method_spec{
				path = [<<"addr2cust">>],
				params = CreateParams},

		{ok, #specs{
			create = Create,
			read = Read,
			update = undefined,
			delete = Delete
		}}.
read(Params) ->
	?log_debug("Params: ~p", [Params]),
	Msisdn = ?gv(msisdn, Params),
	case k_addr2cust:resolve(Msisdn) of
		{error, addr_not_used} ->
			{exception, 'svc0003'};
		{ok, CustomerID, UserID} ->
			Response = prepare(CustomerID, UserID, Msisdn),
			{ok, Response}
	end.

create(Params) ->
	Msisdn = ?gv(msisdn, Params),
	CustomerID = ?gv(customer, Params),
	UserID = ?gv(user, Params),
	case k_addr2cust:link(Msisdn, CustomerID, UserID) of
		ok ->
			Response = prepare(CustomerID, UserID, Msisdn),
			{http_code, 201, Response};
		{error, addr_in_use} ->
			{exception, 'svc0004'}
	end.

update(_Params) ->
	ok.

delete(Params) ->
	Msisdn = ?gv(msisdn, Params),
	k_addr2cust:unlink(Msisdn),
	{http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

prepare(CustomerID, UserID, Msisdn) ->
	[{msisdn, [{addr, Msisdn#addr.addr}, {ton, Msisdn#addr.ton}, {npi, Msisdn#addr.npi}]}, {customer, list_to_binary(k_uuid:to_string(CustomerID))}, {user, UserID}].
