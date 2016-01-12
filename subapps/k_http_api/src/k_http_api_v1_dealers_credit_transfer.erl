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
            {http_code, 400, err_rsp(1)};

        {error, from_customer_not_belong_to_dealer} ->
            {http_code, 400, err_rsp(2)};

        {error, from_customer_doesnot_exist} ->
            {http_code, 400, err_rsp(3)};

        {error, cant_fetch_from_customer, Error} ->
            ?log_error("Fetch from_customer_uuid DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {http_code, 500, err_rsp(4)};

        {error, to_customer_not_belong_to_dealer} ->
            {http_code, 400, err_rsp(5)};

        {error, to_customer_doesnot_exist} ->
            {http_code, 400, err_rsp(6)};

        {error, cant_fetch_to_customer, Error} ->
            ?log_error("Fetch to_customer_uuid DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {http_code, 500, err_rsp(7)};

        {error, cant_create_transaction, Error} ->
            ?log_error("Create credit transfer transaction DB error (~s;~s;~s;~p): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, Error]),
            {http_code, 500, err_rsp(8)};

        {error, TransactionUUID, not_enough_credits_or_customer_not_exist} ->
            ?log_error("Not enough credits or from customer not exist (~s;~s;~s;~p;~s)",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID]),
            {http_code, 400, err_rsp(9, TransactionUUID)};

        {error, TransactionUUID, cant_change_credit_for_from_customer, Error} ->
            ?log_error("Change credit from DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {http_code, 500, err_rsp(10, TransactionUUID)};

        {error, TransactionUUID, cant_change_credit_for_to_customer, Error} ->
            ?log_error("Change credit to DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {http_code, 500, err_rsp(11, TransactionUUID)};

        {error, TransactionUUID, cant_complete_transaction, Error} ->
            ?log_error("Complete credit transfer transaction DB error (~s;~s;~s;~p;~s): ~p",
                [DealerUUID, FromCustomerUUID, ToCustomerUUID, Amount, TransactionUUID, Error]),
            {http_code, 500, err_rsp(12, TransactionUUID)}
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

err_rsp(Code) ->
    err_rsp(Code, undefined).

err_rsp(Code, TransactionUUID) when Code >= 1 andalso Code =< 12 ->

    Description =
    case Code of
        1 -> <<"from_customer_uuid does not have enough credit amount">>;
        2 -> <<"from_customer_uuid does not belong to specified dealer">>;
        3 -> <<"from_customer_uuid does not exist">>;
        4 -> <<"Fetch customer by from_customer_uuid DB error">>;
        5 -> <<"to_customer_uuid does not belong to specified dealer">>;
        6 -> <<"to_customer_uuid does not exist">>;
        7 -> <<"Fetch customer by to_customer_uuid DB error">>;
        8 -> <<"Create credit transfer transaction DB error">>;
        9 -> <<"Not enough credits or from customer not exist">>;
        10 -> <<"Change credit from DB error">>;
        11 -> <<"Change credit to DB error">>;
        12 -> <<"Complete credit transfer transaction DB error">>
    end,

    Rsp0 =
    [
        {error_code, Code},
        {error_description, Description}
    ],

    if
        TransactionUUID =:= undefined ->
            Rsp0;

        true ->
            [{transaction_id, TransactionUUID} | Rsp0]
    end.
