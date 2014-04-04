-module(kelly_http_api_ntw_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

ntw_test_() ->
    {"Network http interface tests",
    {setup,
        fun delete_ntw/0,
        {inorder, [?_test(create_ntw()),
                    ?_test(update_ntw()),
                    ?_test(delete_ntw())]
        }
    }}.

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
    Queries = [
       {id, NtwID},
       {name, <<"test_network">>},
       {country_code, <<"375">>},
       {numbers_len, 12},
       {provider_id, PrvID}],
    Response = ?perform_post(Url, [], <<>>, [{"prefixes", <<"29;33;44">>} | Queries]),
    ?assert_status(201, Response),
    ?assert_json_values(Queries, Response).

update_ntw() ->
    NtwID = ntw_id(),
    Url = ntw_path(NtwID),
    PrvID = <<"0a89542c-5270-11e1-bf27-001d0947ec74">>,
    Queries = [
       {name, <<"new_name">>},
       {country_code, <<"376">>},
       {numbers_len, 13},
       {provider_id, PrvID}],
    Resp = ?perform_put(Url, [], <<>>, Queries),
    ?assert_status(200, Resp),
    ?assert_json_values(Queries, Resp).

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
