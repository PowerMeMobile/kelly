-module(kelly_http_api_addr2cust_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

addr2cust_test_() ->
    {"Add2cust http interface tests",
    {setup,
        fun delete_res/0,
        {inorder, [
            ?_test(create_res()),
            ?_test(delete_res())
        ]}
    }}.

%% ===================================================================
%% AddrToCust Tests
%% ===================================================================

res_id() ->
    <<"999888777666,9,9">>.

res_path() ->
    "http://127.0.0.1:8080/addr2cust".
res_path(Id) ->
    res_path() ++ "/" ++ binary_to_list(Id).

create_res() ->
    Url = res_path(),
    Msisdn = {msisdn, res_id()},
    Query = [
        {customer, <<"feda5822-5271-11e1-bd27-001d0947ec73">>},
        {user, <<"undefined">>}
    ],
    Resp = ?perform_post(Url, [], <<>>, [Msisdn | Query]),
    %?debugFmt("~p~n", [Resp]),
    ?assert_status(201, Resp),

    %% Msisdn2 = {msisdn, [
    %%     {<<"addr">>, <<"375296660001">>},
    %%     {<<"ton">>, 1},
    %%     {<<"npi">>, 1}
    %% ]},
    User2 = {user, null},
    Query2 = lists:keyreplace(user, 1, Query, User2),
    ?assert_json_values(Query2, Resp).

delete_res() ->
    delete_res(res_id()).

delete_res(ResId) ->
    Url = res_path(ResId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
