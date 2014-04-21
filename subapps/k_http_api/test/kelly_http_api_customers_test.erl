-module(kelly_http_api_customers_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

customer_test_() ->
    {"Customer & its users http interface tests",
    {setup,
        fun delete_customer/0,
        {inorder, [
            ?_test(create_customer()),
            ?_test(update_customer()),
            ?_test(create_user()),
            ?_test(update_user()),
            ?_test(delete_user()),
            ?_test(delete_customer())
        ]}
    }}.

%% ===================================================================
%% CUSTOMER Tests
%% ===================================================================

customer_uuid() ->
    <<"8c18fdc4-c4a8-11e3-9f1b-00269e42f7a5">>.

customer_path() ->
    "http://127.0.0.1:8080/customers".
customer_path(Uuid) ->
    customer_path() ++ "/" ++ binary_to_list(Uuid).

create_customer() ->
    Url = customer_path(),
    AllowedSources = {allowed_sources, <<"375296660001,1,1">>},
    DefaultSource = {default_source, <<"375296660001,1,1">>},
    Query = [
        {customer_uuid, customer_uuid()},
        {customer_id, <<"0">>},
        {name, <<"name">>},
        {priority, 1},
        {rps, 1000},
        {receipts_allowed, true},
        {default_validity, <<"000003000000000R">>},
        {max_validity, 259200},
        {default_provider_id, <<"0a89542c-5270-11e1-bf27-001d0947ec73">>},
        {network_map_id, <<"befa8b7c-c4a3-11e3-b670-00269e42f7a5">>},
        {pay_type, <<"postpaid">>},
        {credit, 10000.0},
        {credit_limit, 10000.0},
        {state, <<"active">>}
    ],
    Resp = ?perform_post(Url, [], <<>>, [AllowedSources, DefaultSource | Query]),
    %?debugFmt("~p~n", [Resp]),
    ?assert_status(201, Resp),
    ?assert_json_values(Query, Resp).

update_customer() ->
    Uuid = customer_uuid(),
    Url = customer_path(Uuid),
    AllowedSources = {allowed_sources, <<"375296660002,1,1">>},
    DefaultSource = {default_source, <<"375296660002,1,1">>},
    Query = [
        {customer_id, <<"1">>},
        {name, <<"name-new">>},
        {priority, 2},
        {rps, 500},
        {receipts_allowed, false},
        {default_validity, <<"000004000000000R">>},
        {max_validity, 259201},
        {default_provider_id, <<"0a89542c-5270-11e1-bf27-001d0947ec74">>},
        {network_map_id, <<"cf1563b0-c4a3-11e3-8a61-00269e42f7a5">>},
        {pay_type, <<"prepaid">>},
        {credit, 20000.0},
        {credit_limit, 20000.0},
        {state, <<"blocked">>}
    ],
    Resp = ?perform_put(Url, [], <<>>, [AllowedSources, DefaultSource | Query]),
    %?debugFmt("~p~n", [Resp]),
    ?assert_status(200, Resp),
    ?assert_json_values(Query, Resp).

delete_customer() ->
    delete_customer(customer_uuid()).

delete_customer(CustomerUuid) ->
    Url = customer_path(CustomerUuid),
    delete_req(Url).

%% %% ===================================================================
%% %% Customer USER Tests
%% %% ===================================================================

user_id() -> <<"user">>.

user_path(CustomerId, UserId) ->
    user_path(CustomerId) ++ "/" ++ binary_to_list(UserId).
user_path(CustomerId) ->
    customer_path(CustomerId) ++ "/users".

create_user() ->
    Url = user_path(customer_uuid()),
    Pswd = {password, <<"password">>},
    Query = [
        {id, user_id()},
        {connection_types, <<"transmitter;receiver">>}
    ],
    Resp = ?perform_post(Url, [], <<>>, [Pswd | Query]),
    %?debugFmt("~p~n", [Resp]),
    ?assert_status(201, Resp),
    Query2 = lists:keyreplace(connection_types, 1, Query, {connection_types, [<<"transmitter">>, <<"receiver">>]}),
    ?assert_json_values(Query2, Resp).

update_user() ->
    Url = user_path(customer_uuid(), user_id()),
    Pswd = {password, <<"new-password">>},
    Query = [
        {connection_types, <<"transmitter">>}
    ],
    Resp = ?perform_put(Url, [], <<>>, [Pswd | Query]),
    %?debugFmt("~p~n", [Resp]),
    ?assert_status(200, Resp),
    Query2 = lists:keyreplace(connection_types, 1, Query, {connection_types, [<<"transmitter">>]}),
    ?assert_json_values(Query2, Resp).

delete_user() ->
    delete_user(user_id()).
delete_user(UserId) ->
    Url = user_path(customer_uuid(), UserId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
