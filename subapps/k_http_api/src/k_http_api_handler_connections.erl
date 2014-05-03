-module(k_http_api_handler_connections).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/gateway.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = false, repeated = false, type = integer}
    ],
    Update = [
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = integer},
        #param{name = host, mandatory = false, repeated = false, type = binary},
        #param{name = port, mandatory = false, repeated = false, type = integer},
        #param{name = bind_type, mandatory = false, repeated = false, type = {custom, fun bind_type/1}},
        #param{name = system_id, mandatory = false, repeated = false, type = binary},
        #param{name = password, mandatory = false, repeated = false, type = binary},
        #param{name = system_type, mandatory = false, repeated = false, type = binary},
        #param{name = addr_ton, mandatory = false, repeated = false, type = integer},
        #param{name = addr_npi, mandatory = false, repeated = false, type = integer},
        #param{name = addr_range, mandatory = false, repeated = false, type = binary}
    ],
    Delete = [
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = integer}
    ],
    Create = [
        #param{name = gateway_id, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = false, repeated = false, type = integer},
        #param{name = host, mandatory = true, repeated = false, type = binary},
        #param{name = port, mandatory = true, repeated = false, type = integer},
        #param{name = bind_type, mandatory = true, repeated = false, type = {custom, fun bind_type/1}},
        #param{name = system_id, mandatory = true, repeated = false, type = binary},
        #param{name = password, mandatory = true, repeated = false, type = binary},
        #param{name = system_type, mandatory = true, repeated = false, type = binary},
        #param{name = addr_ton, mandatory = true, repeated = false, type = integer},
        #param{name = addr_npi, mandatory = true, repeated = false, type = integer},
        #param{name = addr_range, mandatory = true, repeated = false, type = binary}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/gateways/:gateway_id/connections/[:id]"
    }}.

read(Params) ->
    ConnectionID = ?gv(id, Params),
    case ConnectionID of
        undefined -> read_all(?gv(gateway_id, Params));
        _ -> read_id(?gv(gateway_id, Params), ConnectionID)
    end.

create(Params) ->
    GtwUuid = ?gv(gateway_id, Params),
    case k_storage_gateways:get_gateway(GtwUuid) of
        {ok, Gtw = #gateway{}} ->
            check_connection_id(Gtw, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update(Params) ->
    GtwUuid = ?gv(gateway_id, Params),
    case k_storage_gateways:get_gateway(GtwUuid) of
        {ok, Gtw = #gateway{}} ->
            update_connection(Gtw, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    GtwUuid = ?gv(gateway_id, Params),
    ConnectionID = ?gv(id, Params),
    ok = k_storage_gateways:del_gateway_connection(GtwUuid, ConnectionID),
    k_snmp:delete_connection(GtwUuid, ConnectionID),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all(GatewayID) ->
    case k_storage_gateways:get_gateway(GatewayID) of
        {ok, #gateway{connections = Connections}} ->
            {ok, Plists} = prepare_connections(Connections),
            ?log_debug("Connections: ~p", [Plists]),
            {http_code, 200, {connections, Plists}};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

read_id(GatewayID, ConnectionID) ->
    case k_storage_gateways:get_gateway(GatewayID) of
        {ok, #gateway{connections = Connections}} ->
            case get_connection(ConnectionID, Connections) of
                undefined ->
                    ?log_debug("Connection [~p] not found", [ConnectionID]),
                    {exception, 'svc0003'};
                Connection = #connection{} ->
                    {ok, [Plist]} = prepare_connections(Connection),
                    ?log_debug("Connection: ~p", [Plist]),
                    {http_code, 200, Plist}
            end;
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update_connection(Gtw, Params) ->
    GtwID = ?gv(gateway_id, Params),
    ConnectionID = ?gv(id, Params),
    #gateway{connections = Connections} = Gtw,
    case get_connection(ConnectionID, Connections) of
        undefined -> {exception, 'svc0003'};
        Conn = #connection{} ->
            NewHost = ?gv(host, Params, Conn#connection.host),
            NewPort = ?gv(port, Params, Conn#connection.port),
            NewBindType = ?gv(bind_type, Params, Conn#connection.bind_type),
            NewSystemID = ?gv(system_id, Params, Conn#connection.system_id),
            NewPassword = ?gv(password, Params, Conn#connection.password),
            NewSystemType = ?gv(system_type, Params, Conn#connection.system_type),
            NewAddrTon = ?gv(addr_ton, Params, Conn#connection.addr_ton),
            NewAddrNpi = ?gv(addr_npi, Params, Conn#connection.addr_npi),
            NewAddrRange = ?gv(addr_range, Params, Conn#connection.addr_range),
            NewConnection = #connection{
                id = ConnectionID,
                host = NewHost,
                port = NewPort,
                bind_type = NewBindType,
                system_id = NewSystemID,
                password = NewPassword,
                system_type = NewSystemType,
                addr_ton = NewAddrTon,
                addr_npi = NewAddrNpi,
                addr_range = NewAddrRange
            },
            {ok, NewConnections} = replace_connection(NewConnection, Connections),
            ok = k_storage_gateways:set_gateway(GtwID, Gtw#gateway{connections = NewConnections}),
            k_snmp:set_connection(GtwID, NewConnection),
            {ok, [Plist]} = prepare_connections(NewConnection),
            ?log_debug("Connection: ~p", [Plist]),
            {http_code, 200, Plist}
    end.

replace_connection(NewConnection, Connections) ->
    replace_connection(NewConnection, Connections, []).
replace_connection(_, [], Connections) ->
    {ok, Connections};
replace_connection(NewConn = #connection{id = ID}, [#connection{id = ID} | Tail], Acc) ->
    replace_connection(NewConn, Tail, [NewConn | Acc]);
replace_connection(NewConn, [Conn | Tail], Acc) ->
    replace_connection(NewConn, Tail, [Conn | Acc]).


get_next_connection_id(Connections) ->
    get_next_connection_id(Connections, 0).
get_next_connection_id([], MaxID) ->
    {ok, MaxID + 1};
get_next_connection_id([#connection{id = ID} | Tail], MaxID) when ID > MaxID ->
    get_next_connection_id(Tail, ID);
get_next_connection_id([_ | Tail], MaxID) ->
    get_next_connection_id(Tail, MaxID).

check_connection_id(Gtw = #gateway{connections = Connections}, Params) ->
    case ?gv(id, Params) of
        undefined ->
            {ok, NextID} = get_next_connection_id(Connections),
            ?log_debug("Next connection id: ~p", [NextID]),
            create_connection(lists:keyreplace(id, 1, Params, {id, NextID}));
        _ ->
            is_connections_exist(Gtw, Params)
    end.

is_connections_exist(#gateway{connections = Connections}, Params) ->
    ConnectionID = ?gv(id, Params),
    case get_connection(ConnectionID, Connections) of
        #connection{} ->
            ?log_debug("Connection [~p] already exist", [ConnectionID]),
            {exception, 'svc0004'};
        undefined ->
            create_connection(Params)
    end.


get_connection(_ID, []) ->
    undefined;
get_connection(ID, [Conn = #connection{id = ID} | _Tail]) ->
    Conn;
get_connection(ID, [_Conn | Tail]) ->
    get_connection(ID, Tail).

create_connection(Params) ->
    ConnectionID = ?gv(id, Params),
    Host = ?gv(host, Params),
    Port = ?gv(port, Params),
    BindType = ?gv(bind_type, Params),
    SystemID = ?gv(system_id, Params),
    Password = ?gv(password, Params),
    SystemType = ?gv(system_type, Params),
    AddrTON = ?gv(addr_ton, Params),
    AddrNPI= ?gv(addr_npi, Params),
    AddrRange = ?gv(addr_range, Params),
    Connection = #connection{
        id          = ConnectionID,
        host        = Host,
        port        = Port,
        bind_type   = BindType,
        system_id   = SystemID,
        password    = Password,
        system_type = SystemType,
        addr_ton    = AddrTON,
        addr_npi    = AddrNPI,
        addr_range  = AddrRange
    },
    GtwUuid = ?gv(gateway_id, Params),
    k_storage_gateways:set_gateway_connection(GtwUuid, Connection),
    k_snmp:set_connection(GtwUuid, Connection),
    {ok, [Plist]} = prepare_connections(Connection),
    ?log_debug("Connection: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare_connections(ConnectionsList) when is_list(ConnectionsList) ->
    prepare_connections(ConnectionsList, []);
prepare_connections(Connection = #connection{}) ->
    prepare_connections([Connection], []).

prepare_connections([], Acc) ->
    {ok, Acc};
prepare_connections([Connection = #connection{} | Rest], Acc) ->
    %% convert connections records to proplists
    Fun = ?record_to_proplist(connection),
    Plist = Fun(Connection),
    prepare_connections(Rest, [Plist | Acc]).

bind_type(TypeBin) ->
    case TypeBin of
        <<"transmitter">> -> transmitter;
        <<"receiver">> -> receiver;
        <<"transceiver">> -> transceiver
    end.
