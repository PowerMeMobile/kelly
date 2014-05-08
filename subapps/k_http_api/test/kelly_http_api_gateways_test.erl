-module(kelly_http_api_gateways_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

gateway_test_() ->
    {"Gateway & its connections http interface tests",
    {setup,
        fun delete_gateway/0,
        {inorder, [
            ?_test(create_gateway()),
            ?_test(update_gateway()),
            ?_test(create_connection()),
            ?_test(update_connection()),
            ?_test(delete_connection()),
            ?_test(delete_gateway())
        ]}
    }}.

%% ===================================================================
%% GATEWAY Tests
%% ===================================================================

gateway_id() ->
    <<"2a4d2aee-c4b1-11e3-9bc1-00269e42f7a5">>.

gateway_path() ->
    "http://127.0.0.1:8080/gateways".
gateway_path(Id) ->
    gateway_path() ++ "/" ++ binary_to_list(Id).

create_gateway() ->
    Url = gateway_path(),
    GatewayId = gateway_id(),
    GatewayName = <<"gateway">>,
    GatewayRPS = 10000,
    Query = [
        {id, GatewayId},
        {name, GatewayName},
        {rps, GatewayRPS}
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Resp),
    ?assert_json_values(Query, Resp).

update_gateway() ->
    GatewayId = gateway_id(),
    Url = gateway_path(GatewayId),
    %% does exist?
    Resp = ?perform_get(Url),
    ?assert_status(200, Resp),
    ?assert_json_value(id, GatewayId, Resp),

    %% update name
    NewName = <<"new_name">>,
    UpdateNameQuery = [{name, NewName}],
    PutResp = ?perform_put(Url, [], <<>>, UpdateNameQuery),
    ?assert_status(200, PutResp),
    ?assert_json_value(name, NewName, PutResp),

    %% update rps
    NewRPS = 12345,
    UpdRPSQuery = [{rps, NewRPS}],
    UpdateRPSResp = ?perform_put(Url, [], <<>>, UpdRPSQuery),
    ?assert_status(200, UpdateRPSResp),
    ?assert_json_value(rps, NewRPS, UpdateRPSResp).

delete_gateway() ->
    delete_gateway(gateway_id()).

delete_gateway(GatewayId) ->
    Url = gateway_path(GatewayId),
    delete_req(Url).

%% ===================================================================
%% Gateway CONNECTION Tests
%% ===================================================================

conn_id() ->
    0.

conn_path(GatewayId, ConnId) ->
    conn_path(GatewayId) ++ "/" ++ integer_to_list(ConnId).
conn_path(GatewayId) ->
    gateway_path(GatewayId) ++ "/connections".

create_connection() ->
    Url = conn_path(gateway_id()),
    ConnId = conn_id(),
    Query = [
        {id, ConnId},
        {host, <<"127.0.0.1">>},
        {port, 8001},
        {bind_type, <<"transmitter">>},
        {system_id, <<"smppclient1">>},
        {password, <<"password">>},
        {system_type, <<"smppclient1">>},
        {addr_ton, 1},
        {addr_npi, 0},
        {addr_range, <<"">>}
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Resp),
    ?assert_json_values(Query, Resp).

update_connection() ->
    ConnId = conn_id(),
    Url = conn_path(gateway_id(), ConnId),
    Query = [
        {id, ConnId},
        {host, <<"127.0.0.2">>},
        {port, 8002},
        {bind_type, <<"receiver">>},
        {system_id, <<"smppclient2">>},
        {password, <<"password2">>},
        {system_type, <<"smppclient2">>},
        {addr_ton, 2},
        {addr_npi, 2},
        {addr_range, <<"">>}
    ],
    Resp = ?perform_put(Url, [], <<>>, Query),
    ?assert_status(200, Resp),
    ?assert_json_values(Query, Resp).

delete_connection() ->
    delete_connection(conn_id()).
delete_connection(ConnId) ->
    Url = conn_path(gateway_id(), ConnId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
