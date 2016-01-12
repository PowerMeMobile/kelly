-module(k_http_api_exceptions).

-export([
    exception_body_and_code/1,
    exception_body_and_code/2
]).

-include("http_service_exceptions.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec exception_body_and_code(atom()) -> ignore.
exception_body_and_code(ExceptionCode) ->
    exception_body_and_code(ExceptionCode, []).


-spec exception_body_and_code(atom(), list(term())) -> ignore.
exception_body_and_code(ExceptionCode = ?SVC0101, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"from_customer_uuid does not have enough credit amount">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0102, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"from_customer_uuid does not belong to specified dealer">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0103, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"from_customer_uuid does not exist">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0104, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Fetch customer by from_customer_uuid DB error">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(ExceptionCode = ?SVC0105, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"to_customer_uuid does not belong to specified dealer">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0106, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"to_customer_uuid does not exist">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0107, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Fetch customer by to_customer_uuid DB error">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(ExceptionCode = ?SVC0108, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Create credit transfer transaction DB error">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(ExceptionCode = ?SVC0109, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Not enough credits or from customer not exist (%1)">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(ExceptionCode = ?SVC0110, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Change credit from DB error (%1)">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(ExceptionCode = ?SVC0111, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Change credit to DB error (%1)">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(ExceptionCode = ?SVC0112, Variables) ->
    MessageID = list_to_binary(string:to_upper(atom_to_list(ExceptionCode))),
    Text = <<"Complete credit transfer transaction DB error (%1)">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 500};

exception_body_and_code(Exception, _Variables) ->
    {error, {no_such_exception, Exception}}.


exception_body(MessageID, Text, Variables) ->
    Body = [
        {<<"request_error">>, [
            {<<"service_exception">>, [
                {<<"message_id">>, MessageID},
                {<<"text">>, Text},
                {<<"variables">>, Variables}
            ]}
        ]}
    ],
    gen_http_api_converter:process(Body, <<"json">>).


