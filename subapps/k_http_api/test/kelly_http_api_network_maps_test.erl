-module(kelly_http_api_network_maps_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

network_map_test_() ->
    {"Network map  http interface tests",
    {setup,
        fun delete_network_map/0,
        {inorder, [
            ?_test(create_network_map_wo_network_ids_fail()),
            ?_test(create_network_map()),
            ?_test(update_network_map()),
            ?_test(delete_network_map())
        ]}
    }}.

%% ===================================================================
%% NETWORK_MAP Tests
%% ===================================================================

network_map_uuid() ->
    <<"62ce045e-c4b3-11e3-9d4c-00269e42f7a5">>.

network_map_path() ->
    "http://127.0.0.1:8080/network_maps".
network_map_path(Uuid) ->
    network_map_path() ++ "/" ++ binary_to_list(Uuid).

create_network_map_wo_network_ids_fail() ->
    Url = network_map_path(),
    Query = [
        {id, network_map_uuid()},
        {name, <<"country">>}
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    %% no network_ids field
    ?assert_status(400, Resp).

create_network_map() ->
    Url = network_map_path(),
    Query = [
        {id, network_map_uuid()},
        {name, <<"country">>},
        {network_ids,
            <<"80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5">>
        }
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Resp),

    NetworkIds = {network_ids, [
        <<"80755f2a-c4b3-11e3-b7a4-00269e42f7a5a">>,
        <<"977ffa0e-c4b3-11e3-a01f-00269e42f7a5">>
    ]},
    Query2 = lists:keyreplace(network_ids, 1, Query, NetworkIds),
    ?assert_json_values(Query2, Resp).

update_network_map() ->
    Id = network_map_uuid(),
    Url = network_map_path(Id),
    Query = [
        {id, Id},
        {name, <<"country-new">>},
        {network_ids,
            <<"80755f2a-c4b3-11e3-b7a4-00269e42f7a5a">>
        }
    ],
    Resp = ?perform_put(Url, [], <<>>, Query),
    ?assert_status(200, Resp),

    NetworkIds = {network_ids, [<<"80755f2a-c4b3-11e3-b7a4-00269e42f7a5a">>]},
    Query2 = lists:keyreplace(network_ids, 1, Query, NetworkIds),
    ?assert_json_values(Query2, Resp).

delete_network_map() ->
    delete_network_map(network_map_uuid()).

delete_network_map(Id) ->
    Url = network_map_path(Id),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
