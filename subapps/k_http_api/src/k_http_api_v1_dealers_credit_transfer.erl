-module(k_http_api_v1_dealers_credit_transfer).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include("http_service_exceptions.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Create = [
        #param{name = dealer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = from_customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = to_customer_uuid, mandatory = true, repeated = false, type = uuid},
        #param{name = amount, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = [],
        update = [],
        delete = [],
        route = "/v1/dealers/[:dealer_uuid]/credit_transfer"
    }}.


create(Params) ->
    DealerUUID = ?gv(dealer_uuid, Params),
    FromCustomerUUID = ?gv(from_customer_uuid, Params),
    ToCustomerUUID = ?gv(to_customer_uuid, Params),
    Amount = ?gv(amount, Params),
    case k_storage_customers:transfer_credit(DealerUUID,
                                             FromCustomerUUID,
                                             ToCustomerUUID,
                                             Amount) of
        {ok, TransactionUUID} ->
            Resp = [{transaction_id, TransactionUUID}],
            {http_code, 200, Resp};

        {error, not_enough_credit_amount} ->
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0101),
            {http_code, Code, Rsp};

        {error, from_customer_not_belong_to_dealer} ->
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0102),
            {http_code, Code, Rsp};

        {error, from_customer_doesnot_exist} ->
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0103),
            {http_code, Code, Rsp};

        {error, cant_fetch_from_customer, Error} ->
            ?log_error("Fetch from_customer_uuid DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0104),
            {http_code, Code, Rsp};

        {error, to_customer_not_belong_to_dealer} ->
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0105),
            {http_code, Code, Rsp};

        {error, to_customer_doesnot_exist} ->
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0106),
            {http_code, Code, Rsp};

        {error, cant_fetch_to_customer, Error} ->
            ?log_error("Fetch to_customer_uuid DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0107),
            {http_code, Code, Rsp};

        {error, cant_create_transaction, Error} ->
            ?log_error("Create credit transfer transaction DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0108),
            {http_code, Code, Rsp};

        {error, TransactionUUID, not_enough_credits_or_customer_not_exist} ->
            ?log_error("Not enough credits or from customer not exist (~s;~s;~s;~p;~s)",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0109, [TransactionUUID]),
            {http_code, Code, Rsp};

        {error, TransactionUUID, cant_change_credit_for_from_customer, Error} ->
            ?log_error("Change credit from DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0110, [TransactionUUID]),
            {http_code, Code, Rsp};

        {error, TransactionUUID, cant_change_credit_for_to_customer, Error} ->
            ?log_error("Change credit to DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0111, [TransactionUUID]),
            {http_code, Code, Rsp};

        {error, TransactionUUID, cant_complete_transaction, Error} ->
            ?log_error("Complete credit transfer transaction DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {ok, Rsp, Code} = k_http_api_exceptions:exception_body_and_code(?SVC0112, [TransactionUUID]),
            {http_code, Code, Rsp}
    end.


read(_Params) ->
    {http_code, 400}.


update(_Params) ->
    {http_code, 400}.


delete(_Params) ->
    {http_code, 400}.

%% ===================================================================
%% Internal
%% ===================================================================
