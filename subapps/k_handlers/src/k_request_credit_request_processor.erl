-module(k_request_credit_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_request_credit_request_dto{}) ->
    {ok, #k1api_request_credit_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId       = ReqDTO#k1api_request_credit_request_dto.id,
    CustomerId  = ReqDTO#k1api_request_credit_request_dto.customer_id,
    CreditRequested = ReqDTO#k1api_request_credit_request_dto.credit,

    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, Customer} ->
            CustomerUuid = Customer#customer.customer_uuid,
            Credit = Customer#customer.credit,
            CreditLimit = Customer#customer.credit_limit,
            Credit2 = Credit - CreditRequested,
            {Result, CreditLeft} =
                case Credit2 < -CreditLimit of
                    true ->
                        {denied, Credit};
                    false ->
                        ok = k_storage_customers:reduce_credit(CustomerUuid, CreditRequested),
                        {allowed, Credit2}
                end,
            {ok, #k1api_request_credit_response_dto{
                id = ReqId,
                result = Result,
                credit_left = CreditLeft
            }};
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================
