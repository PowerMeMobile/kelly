-module(kelly_http_api_prv_test).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

prv_test_() ->
    {"Provider http interface tests",
        {setup,
            fun delete_prv/0,
            {inorder, [
                ?_test(create_prv()),
                ?_test(update_prv()),
                ?_test(delete_prv())
            ]}
        }
    }.

%% ===================================================================
%% PROVIDER Tests
%% ===================================================================

prv_id() ->
    <<"0a89542c-5270-11e1-bf27-001d0947ec73">>.

prv_path() ->
    "http://127.0.0.1:8080/providers".

prv_path(ID) ->
    prv_path() ++ "/" ++ binary_to_list(ID).

create_prv() ->
    Url = prv_path(),
    PrvID = prv_id(),
    Queries = [
        {id, PrvID},
        {name, <<"test_provider">>},
        {gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>},
        {bulk_gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>},
        {receipts_supported, true},
        {sms_add_points, 0.0}
    ],
    Response = ?perform_post(Url, [], <<>>, Queries),
    ?assert_status(201, Response),
    ?assert_json_values(Queries, Response).

update_prv() ->
    PrvID = prv_id(),
    Url = prv_path(PrvID),
    Queries = [
        {name, <<"new_provider_name">>},
        {gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace6">>},
        {bulk_gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace6">>},
        {receipts_supported, false},
        {sms_add_points, 1.0}
    ],
    Response = ?perform_put(Url, [], <<>>, Queries),
    ?assert_status(200, Response),
    ?assert_json_values(Queries, Response).

delete_prv() ->
    delete_prv(prv_id()).

delete_prv(PrvID) ->
    Url = prv_path(PrvID),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Response = ?perform_delete(Url),
    ?assert_status(204, Response).
