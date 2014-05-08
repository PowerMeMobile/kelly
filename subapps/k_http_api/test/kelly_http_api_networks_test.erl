-module(kelly_http_api_networks_test).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

network_test_() ->
    {"Network http interface tests",
        {setup,
            fun delete_network/0,
            {inorder, [
                ?_test(create_network()),
                ?_test(update_network()),
                ?_test(delete_network())
            ]}
        }
    }.

%% ===================================================================
%% NETWORK Tests
%% ===================================================================

network_id() ->
    <<"920a009a-5270-11e1-b961-001d0947ec73">>.

network_path() ->
    "http://127.0.0.1:8080/networks".

network_path(Id) ->
    network_path() ++ "/" ++ binary_to_list(Id).

create_network() ->
    Url = network_path(),
    NetworkId = network_id(),
    ProviderId = <<"0a89542c-5270-11e1-bf27-001d0947ec73">>,
    Query = [
        {id, NetworkId},
        {name, <<"network">>},
        {country, <<"country">>},
        {hex_code, <<"FF">>},
        {country_code, <<"375">>},
        {number_len, 12},
        {prefixes, <<"29;33;44">>},
        {gmt_diff, <<"+2">>},
        {dst, <<"5,7,3">>},
        {provider_id, ProviderId},
        {is_home, true},
        {sms_points, 1.0},
        {sms_mult_points, 1.0}
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Resp),
    Query2 = lists:keyreplace(prefixes, 1, Query, {prefixes, [<<"29">>, <<"33">>, <<"44">>]}),
    ?assert_json_values(Query2, Resp).

update_network() ->
    NetworkId = network_id(),
    Url = network_path(NetworkId),
    ProviderId = <<"0a89542c-5270-11e1-bf27-001d0947ec74">>,
    Query = [
        {id, NetworkId},
        {name, <<"new_network">>},
        {country, <<"new_country">>},
        {hex_code, <<"AA">>},
        {country_code, <<"376">>},
        {number_len, 13},
        {prefixes, <<"33;44">>},
        {gmt_diff, <<"-2">>},
        {dst, <<"6,5,2">>},
        {provider_id, ProviderId},
        {is_home, false},
        {sms_points, 0.0},
        {sms_mult_points, 0.0}
    ],
    Resp = ?perform_put(Url, [], <<>>, Query),
    ?assert_status(200, Resp),
    Query2 = lists:keyreplace(prefixes, 1, Query, {prefixes, [<<"33">>, <<"44">>]}),
    ?assert_json_values(Query2, Resp).

delete_network() ->
    delete_network(network_id()).

delete_network(NetworkId) ->
    Url = network_path(NetworkId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
