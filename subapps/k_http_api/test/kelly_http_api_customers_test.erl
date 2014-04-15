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
    <<"feda5822-5271-11e1-bd27-001d0947ec73">>.

customer_path() ->
    "http://127.0.0.1:8080/customers".
customer_path(Uuid) ->
    customer_path() ++ "/" ++ binary_to_list(Uuid).

create_customer() ->
    Url = customer_path(),
    Networks = {networks, <<"920a009a-5270-11e1-b961-001d0947ec73;3b25cd8e-5eca-11e1-bf77-00269e42f7a5">>},
    Originators = {originators, <<"375296660001,1,1">>},
    DefaultOriginator = {default_originator, <<"375296660001,1,1">>},
    Queries = [
        {customer_uuid, customer_uuid()},
        {customer_id, <<"0">>},
        {name, <<"name">>},
        {receipts_allowed, true},
        {default_validity, <<"000003000000000R">>},
        {max_validity, 259200},
        {default_provider_id, <<"0a89542c-5270-11e1-bf27-001d0947ec73">>},
        {pay_type, <<"postpaid">>},
        {state, 1}
    ],
    Response = ?perform_post(Url, [], <<>>, [Networks, Originators, DefaultOriginator | Queries]),
    ?assert_status(201, Response),
    ?assert_json_values(Queries, Response).

update_customer() ->
    Uuid = customer_uuid(),
    Url = customer_path(Uuid),
    Networks = {networks, <<"920a009a-5270-11e1-b961-001d0947ec73;3b25cd8e-5eca-11e1-bf77-00269e42f7a5">>},
    Originators = {originators, <<"375296660002,1,1">>},
    DefaultOriginator = {default_originator, <<"375296660002,1,1">>},
    Queries = [
        {customer_id, <<"1">>},
        {name, <<"name-new">>},
        {receipts_allowed, false},
        {default_validity, <<"000004000000000R">>},
        {max_validity, 259201},
        {default_provider_id, <<"0a89542c-5270-11e1-bf27-001d0947ec74">>},
        {pay_type, <<"prepaid">>},
        {state, 0}
    ],
    Response = ?perform_put(Url, [], <<>>, [Networks, Originators, DefaultOriginator | Queries]),
    ?assert_status(200, Response),
    ?assert_json_values(Queries, Response).

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
    PermittedTypes = {bind_types, <<"transmitter;receiver;transceiver">>},
    Queries = [{id, user_id()}],
    Resp = ?perform_post(Url, [], <<>>, [PermittedTypes, Pswd | Queries]),
    ?assert_status(201, Resp),
    ?assert_json_values(Queries, Resp).

update_user() ->
    Url = user_path(customer_uuid(), user_id()),
    Pswd = {password, <<"new-password">>},
    PermittedTypes = {bind_types, <<"transmitter">>},
    Queries = [],
    Resp = ?perform_put(Url, [], <<>>, [PermittedTypes, Pswd | Queries]),
    ?assert_status(200, Resp),
    ?assert_json_values(Queries, Resp).

delete_user() ->
    delete_user(user_id()).
delete_user(UserId) ->
    Url = user_path(customer_uuid(), UserId),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Response = ?perform_delete(Url),
    ?assert_status(204, Response).
