-module(k_request_credit_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/customer.hrl").

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

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
            {Result, CreditLeft} = request_credit(Credit, CreditLimit, CreditRequested),
            case Result of
                allowed ->
                    ok = k_storage_customers:reduce_credit(CustomerUuid, CreditRequested);
                _ ->
                    nop
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

request_credit(Credit, CreditLimit, CreditRequested) ->
    Credit2 = Credit - CreditRequested,
    CreditLeft = Credit2 + CreditLimit,
    case CreditLeft < 0 of
        true ->
            {denied, Credit};
        false ->
            {allowed, CreditLeft}
    end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

request_credit_test_() -> [
    ?_assertEqual({allowed, 8}, request_credit(10,  0,  2)),
    ?_assertEqual({allowed, 0}, request_credit(10,  0, 10)),
    ?_assertEqual({denied, 10}, request_credit(10,  0, 12)),
    ?_assertEqual({allowed, 8}, request_credit( 0, 10,  2))
].

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================