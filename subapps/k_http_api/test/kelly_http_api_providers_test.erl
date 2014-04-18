-module(kelly_http_api_providers_test).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

provider_test_() ->
    {"Provider http interface tests",
        {setup,
            fun delete_provider/0,
            {inorder, [
                ?_test(create_provider()),
                ?_test(update_provider()),
                ?_test(delete_provider())
            ]}
        }
    }.

%% ===================================================================
%% PROVIDER Tests
%% ===================================================================

provider_id() ->
    <<"c973e494-c4a4-11e3-8684-00269e42f7a5">>.

provider_path() ->
    "http://127.0.0.1:8080/providers".

provider_path(Id) ->
    provider_path() ++ "/" ++ binary_to_list(Id).

create_provider() ->
    Url = provider_path(),
    ProviderId = provider_id(),
    Query = [
        {id, ProviderId},
        {name, <<"test_provider">>},
        {gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>},
        {bulk_gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>},
        {receipts_supported, true},
        {sms_add_points, 0.0}
    ],
    Resp = ?perform_post(Url, [], <<>>, Query),
    ?assert_status(201, Resp),
    ?assert_json_values(Query, Resp).

update_provider() ->
    ProviderId = provider_id(),
    Url = provider_path(ProviderId),
    Query = [
        {name, <<"new_provider_name">>},
        {gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace6">>},
        {bulk_gateway_id, <<"7dc235d0-c938-4b66-8f8c-c9037c7eace6">>},
        {receipts_supported, false},
        {sms_add_points, 1.0}
    ],
    Resp = ?perform_put(Url, [], <<>>, Query),
    ?assert_status(200, Resp),
    ?assert_json_values(Query, Resp).

delete_provider() ->
    delete_provider(provider_id()).

delete_provider(ProviderId) ->
    Url = provider_path(ProviderId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
