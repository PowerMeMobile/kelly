-module(k_j3_support).

-export([
    reconfigure/0
]).

-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/gateway.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec reconfigure() -> ok.
reconfigure() ->
    {ok, Customers} = k_storage_customers:get_customers(),
    [k_snmp:set_customer(
        C#customer.customer_uuid, C#customer.rps, C#customer.priority
    ) || C <- Customers],
    {ok, Gtws} = k_storage_gateways:get_gateways(),
    [set_gtw(Gtw) || Gtw <- Gtws],
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

set_gtw(Gtw) ->
    GtwId = Gtw#gateway.id,
    k_snmp:set_gateway(GtwId, Gtw#gateway.name, Gtw#gateway.rps),
    [k_snmp:set_connection(GtwId, Conn) || Conn <- Gtw#gateway.connections],
    [k_snmp:set_setting(GtwId, Setting) || Setting <- Gtw#gateway.settings].
