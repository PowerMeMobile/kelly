-module(kelly_http_api_ntw_test).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

ntw_test_() ->
    {"Network http interface tests",
        {setup,
            fun delete_ntw/0,
            {inorder, [
                ?_test(create_ntw()),
                ?_test(update_ntw()),
                ?_test(delete_ntw())
            ]}
        }
    }.

%% ===================================================================
%% NETWORK Tests
%% ===================================================================

ntw_id() ->
    <<"920a009a-5270-11e1-b961-001d0947ec73">>.

ntw_path() ->
    "http://127.0.0.1:8080/networks".

ntw_path(ID) ->
    ntw_path() ++ "/" ++ binary_to_list(ID).

create_ntw() ->
    Url = ntw_path(),
    NtwID = ntw_id(),
    PrvID = <<"0a89542c-5270-11e1-bf27-001d0947ec73">>,
    Query = [
        {id, NtwID},
        {name, <<"network">>},
        {country, <<"country">>},
        {hex_code, <<"FF">>},
        {country_code, <<"375">>},
        {number_len, 12},
        {prefixes, <<"29;33;44">>},
        {gmt_diff, <<"+2">>},
        {dst, <<"5,7,3">>},
        {provider_id, PrvID},
        {is_home, true},
        {sms_points, 1.0},
        {sms_mult_points, 1.0}
    ],
    Response = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Response),
    Query2 = lists:keyreplace(prefixes, 1, Query,
        {prefixes, [<<"29">>, <<"33">>, <<"44">>]}),
    ?assert_json_values(Query2, Response).

update_ntw() ->
    NtwID = ntw_id(),
    Url = ntw_path(NtwID),
    PrvID = <<"0a89542c-5270-11e1-bf27-001d0947ec74">>,
    Query = [
        {name, <<"new_network">>},
        {country, <<"new_country">>},
        {hex_code, <<"AA">>},
        {country_code, <<"376">>},
        {number_len, 13},
        {prefixes, <<"33;44">>},
        {gmt_diff, <<"-2">>},
        {dst, <<"6,5,2">>},
        {provider_id, PrvID},
        {is_home, false},
        {sms_points, 0.0},
        {sms_mult_points, 0.0}
    ],
    Resp = ?perform_put(Url, [], <<>>, Query),
    ?assert_status(200, Resp),
    Query2 = lists:keyreplace(prefixes, 1, Query,
        {prefixes, [<<"33">>, <<"44">>]}),
    ?assert_json_values(Query2, Resp).

delete_ntw() ->
    delete_ntw(ntw_id()).

delete_ntw(NtwID) ->
    Url = ntw_path(NtwID),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Response = ?perform_delete(Url),
    ?assert_status(204, Response).
