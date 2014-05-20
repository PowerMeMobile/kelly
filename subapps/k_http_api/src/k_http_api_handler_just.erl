-module(k_http_api_handler_just).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/gateway.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    {ok, #specs{
        create = undefined,
        read = [],
        update = undefined,
        delete = undefined,
        route = "/just/reconfigure"
    }}.

read(_Params) ->
    {ok, Customers} = k_storage_customers:get_customers(),
    [k_snmp:set_customer(
        C#customer.customer_uuid, C#customer.rps, C#customer.priority
    ) || C <- Customers],
    {ok, Gtws} = k_storage_gateways:get_gateways(),
    [set_gtw(Gtw) || Gtw <- Gtws],
    {ok, {result, ok}}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

set_gtw(Gtw) ->
    GtwId = Gtw#gateway.id,
    k_snmp:set_gateway(GtwId, Gtw#gateway.name, Gtw#gateway.rps),
    [k_snmp:set_connection(GtwId, Conn) || Conn <- Gtw#gateway.connections],
    [k_snmp:set_setting(GtwId, Setting) || Setting <- Gtw#gateway.settings].
