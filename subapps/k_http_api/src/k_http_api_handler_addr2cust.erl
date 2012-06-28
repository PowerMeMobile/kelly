-module(k_http_api_handler_addr2cust).

-behaviour(gen_cowboy_restful).

-export([init/3, handle/3, terminate/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include("gen_cowboy_restful_spec.hrl").

-record(state, {
}).

%%% REST parameters
-record(get, {
	addr = {mandatory, <<"addr">>, list},
	ton = {mandatory, <<"ton">>, integer},
	npi = {mandatory, <<"npi">>, integer}
}).

-record(create, {
	addr = {mandatory, <<"addr">>, list},
	ton = {mandatory, <<"ton">>, integer},
	npi = {mandatory, <<"npi">>, integer},
	cid = {mandatory, <<"cid">>, list},
	user = {mandatory, <<"user">>, list}
}).

-record(update, {
}).

-record(delete, {
	addr = {mandatory, <<"addr">>, list},
	ton = {mandatory, <<"ton">>, integer},
	npi = {mandatory, <<"npi">>, integer}
}).

init(_Req, 'GET', _Path) ->
	{ok, #get{}, #state{}};

init(_Req, 'POST', _Path) ->
	{ok, #create{}, #state{}};

init(_Req, 'PUT', _Path) ->
	{ok, #update{}, #state{}};

init(_Req, 'DELETE', _Path) ->
	{ok, #delete{}, #state{}};

init(_Req, HttpMethod, Path) ->
	?log_error("bad_request~nHttpMethod: ~p~nPath: ~p", [HttpMethod, Path]),
	{error, bad_request}.

handle(_Req, #get{
				addr = Addr,
				ton = Ton,
				npi = Npi
			}, State = #state{}) ->
	Response = k_addr2cust:resolve(#addr{addr=Addr, ton=Ton, npi=Npi}),
	{ok, Response, State};

handle(_Req, #create{
				addr = Addr,
				ton = Ton,
				npi = Npi,
				cid = CustID
					}, State = #state{}) ->
	UserID = undefined,
	Result = k_addr2cust:link(#addr{addr=Addr, ton=Ton, npi=Npi}, CustID, UserID),
	{ok, {result, Result}, State};

handle(_Req, #update{}, State = #state{}) ->
	{ok, {result, error}, State};

handle(_Req, #delete{
				addr = Addr,
				ton = Ton,
				npi = Npi
					}, State = #state{}) ->
	Result = k_addr2cust:unlink(#addr{addr=Addr, ton=Ton, npi=Npi}),
	{ok, {result, Result}, State}.

terminate(_Req, _State = #state{}) ->
    ok.
