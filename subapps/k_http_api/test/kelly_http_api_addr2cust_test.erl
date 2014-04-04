-module(kelly_http_api_addr2cust_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

addr2cust_test_() ->
    {"Add2cust http interface tests",
    {setup,
        fun delete_res/0,
        {inorder, [?_test(create_res()),
                    ?_test(delete_res())]
        }
    }}.

%% ===================================================================
%% AddrToCust Tests
%% ===================================================================

res_id() -> <<"375296660001,1,1">>.

res_path() ->
    "http://127.0.0.1:8080/addr2cust".
res_path(ID) ->
    res_path() ++ "/" ++ binary_to_list(ID).

create_res() ->
    Url = res_path(),
    Msisdn = {msisdn, res_id()},
    Queries = [
        {customer, <<"feda5822-5271-11e1-bd27-001d0947ec73">>},
        {user, <<"undefined">>}],
    Response = ?perform_post(Url, [], <<>>, [Msisdn | Queries]),
    ?assert_status(201, Response),
    ?assert_json_values(Queries, Response).

delete_res() ->
    delete_res(res_id()).

delete_res(ResID) ->
    Url = res_path(ResID),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Response = ?perform_delete(Url),
    ?assert_status(204, Response).
