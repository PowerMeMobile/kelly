-module(k_http_api_handler_addr2cust).

-behaviour(gen_http_api).

-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/utils.hrl").
-include_lib("k_mailbox/include/address.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
	Read = [
		#param{name = msisdn, mandatory = false, repeated = false, type = {custom, fun decode_addr/1}},
		#param{name = customer, mandatory = false, repeated = false, type = binary},
		#param{name = user, mandatory = true, repeated = false, type = binary}
	],
	Delete = [
		#param{name = msisdn, mandatory = true, repeated = false, type = {custom, fun decode_addr/1}}
	],
	Create = [
		#param{name = msisdn, mandatory = true, repeated = false, type = {custom, fun decode_addr/1}},
		#param{name = customer,	mandatory = true, repeated = false,	type = binary},
		#param{name = user, mandatory = true, repeated = false, type = binary}
	],
	{ok, #specs{
		create = Create,
		read = Read,
		update = undefined,
		delete = Delete,
		route = "/addr2cust/[:msisdn]"
	}}.

read(Params) ->
	case ?gv(msisdn, Params) of
		undefined -> get_customer_user_msisdns(Params);
		Msisdn -> get_msisdn(Msisdn)
	end.

get_customer_user_msisdns(Params) ->
	Customer = ?gv(customer, Params),
	User = ?gv(user, Params),
	{ok, MsisdnsList} = k_addr2cust:available_addresses(Customer, User),
	Response = prepare_msisdns(Customer, User, MsisdnsList),
	{ok, Response}.

get_msisdn(Msisdn) ->
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
	[{msisdn, [	{addr, Msisdn#addr.addr},
				{ton, Msisdn#addr.ton},
				{npi, Msisdn#addr.npi}]},
	{customer, CustomerID},
	{user, UserID}].

prepare_msisdns(CustomerID, UserID, Msisdns) ->
	[{msisdns, [[{addr, Msisdn#addr.addr},
				{ton, Msisdn#addr.ton},
				{npi, Msisdn#addr.npi}] || Msisdn <- Msisdns]},
	{customer, CustomerID},
	{user, UserID}].

decode_addr(AddrBin) ->
	AddrString = binary_to_list(AddrBin),
	[Addr, Ton, Npi] = string:tokens(AddrString, ","),
	#addr{
		addr = list_to_binary(Addr),
		ton = list_to_integer(Ton),
		npi = list_to_integer(Npi)
	}.
