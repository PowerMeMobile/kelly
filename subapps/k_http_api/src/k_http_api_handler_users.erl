-module(k_http_api_handler_users).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
	cstid :: string(),
	usrid :: string(),
	customer :: #customer{}
}).

%%% REST parameters

-record(get, {}).

-record(create, {
	id			= {mandatory, <<"id">>, list},
	pswd		= {mandatory, <<"pswd">>, list},
	smpp_types 	= {mandatory, <<"smpp_types">>, list} %% "transmitter,receiver,transceiver"
}).

-record(update, {
	pswd		= {optional, <<"pswd">>, list},
	smpp_types 	= {optional, <<"smpp_types">>, list} %% "transmitter,receiver,transceiver"
}).

-record(delete, {}).

init(_Req, 'GET', [_, CstIdBin, _]) ->
	CstId = binary_to_list(CstIdBin),
	{ok, #get{}, #state{cstid = CstId, usrid = all}};

init(_Req, 'GET', [_, CstIdBin, _, UserIdBin]) ->
	UserId = binary_to_list(UserIdBin),
	CstId = binary_to_list(CstIdBin),
	{ok, #get{}, #state{cstid = CstId, usrid = UserId}};

init(_Req, 'POST', [_, CstIdBin, _]) ->
	CstId = binary_to_list(CstIdBin),
	{ok, #create{}, #state{cstid = CstId}};

init(_Req, 'PUT', [_, CstIdBin, _, UserIdBin]) ->
	CstId = binary_to_list(CstIdBin),
	UserId = binary_to_list(UserIdBin),
	{ok, #update{}, #state{cstid = CstId, usrid = UserId}};

init(_Req, 'DELETE', [_, CstIdBin, _, UserIdBin]) ->
	CstId = binary_to_list(CstIdBin),
	UserId = binary_to_list(UserIdBin),
	{ok, #delete{}, #state{cstid = CstId, usrid = UserId}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(Req, #get{}, State = #state{cstid = CustID}) ->
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		get_customer_user(Req, State#state{customer = Customer});
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(Req, Create = #create{}, State = #state{cstid = CustID}) ->
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		create_user(Req, Create, State#state{customer = Customer});
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(Req, Update = #update{}, State = #state{cstid = CustID}) ->
	case k_aaa:get_customer_by_id(CustID) of
		{ok, Customer = #customer{}} ->
	   		update_user(Req, Update, State#state{customer = Customer});
		{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end;

handle(_Req, #delete{}, State = #state{cstid = CstId, usrid = UserId}) ->
	case k_aaa:del_customer_user(CstId, UserId) of
		{error, no_entry} ->
			?log_warn("Customer [~p] not found", [CstId]),
			{exception, 'svc0003', [], State};
		ok ->
			{http_code, 204, State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end.

terminate(_Req, _State = #state{}) ->
    ok.

%%% Local functions

%% convert "transmitter,receiver,transceiver"
%% to [transmitter, receiver, transmitter]
convert(undefined) -> undefined;
convert(SmppTypesString) ->
	Tokens = string:tokens(SmppTypesString, ","),
	convert(Tokens, []).

convert([], Acc) ->
	Acc;
convert([Type | Rest], Acc) ->
	case Type of
		"transmitter" -> convert(Rest, [transmitter | Acc]);
		"receiver" -> convert(Rest, [receiver | Acc]);
		"transceiver" -> convert(Rest, [transceiver | Acc]);
		_Any -> convert(Rest, Acc)
	end.


translate(Proplist) ->
	translate(Proplist, []).
translate([], Acc) ->
	lists:reverse(Acc);
translate([{Name, Value} | Tail], Acc) ->
	translate(Tail, [{translate_name(Name), Value} | Acc]).

translate_name(permitted_smpp_types) ->
	smpp_types;
translate_name(Name) ->
	Name.

prepare_users(Users) ->
	UserFun = ?record_to_proplist(user),
	{ok, lists:map(fun(User)->
		UserPropList = UserFun(User),
		translate(proplists:delete(pswd_hash, UserPropList))
		end, Users)}.

create_user(_Req, Create = #create{id = UserID}, State = #state{customer = Customer}) ->
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, #user{}} ->
			{exception, 'svc0004', [], State};
		{error, no_entry} ->
			#create{
				pswd		= Pass,
				smpp_types 	= TypesString
				} = Create,
			User = #user{
				id 						= UserID,
				pswd_hash 				= crypto:sha(Pass),
				permitted_smpp_types 	= convert(TypesString)
				},
			ok = k_aaa:set_customer_user(User, Customer#customer.uuid),
			{ok, UserPropList} = prepare_users([User]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{http_code, 201, UserPropList, State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end.

update_user(_Req, Update, State = #state{usrid = UserID, customer = Customer}) ->

	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, User} ->
			Updated = #user{
				id = UserID,
				pswd_hash = resolve_pass(Update#update.pswd, User#user.pswd_hash),
				permitted_smpp_types = resolve(convert(Update#update.smpp_types), User#user.permitted_smpp_types)
			},
			ok = k_aaa:set_customer_user(Updated, Customer#customer.uuid),
			{ok, [UserPropList]} = prepare_users([Updated]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{ok, UserPropList, State};
   	  	{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end.

get_customer_user(_Req, State = #state{usrid = all, customer = Customer}) ->
	#customer{users = Users} = Customer,
	{ok, UserPropList} = prepare_users(Users),
	?log_debug("UserPropList: ~p", [UserPropList]),
	{ok, UserPropList, State};

get_customer_user(_Req, State = #state{usrid = UserID, customer = Customer}) ->
	case k_aaa:get_customer_user(Customer, UserID) of
		{ok, User} ->
			{ok, [UserPropList]} = prepare_users([User]),
			?log_debug("UserPropList: ~p", [UserPropList]),
			{ok, UserPropList, State};
   	  	{error, no_entry} ->
			{exception, 'svc0003', [], State};
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			{http_code, 500, State}
	end.

resolve_pass(undefined, Pass) ->
	Pass;
resolve_pass(NewPass, _Pass) ->
	crypto:sha(NewPass).

resolve(undefined, Value) ->
	Value;
resolve(NewValue, _Value) ->
	NewValue.
