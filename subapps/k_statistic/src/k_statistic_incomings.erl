-module(k_statistic_incomings).

-export([
    get_all/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").

-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all([{atom(), term()}]) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_all(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    State = ?gv(state, Params, all),
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),
    case k_storage_mailbox:get_incomings(
            From, To, CustomerUuid, UserId, State, Skip, Limit) of
        {ok, Incomings} ->
            Uuids = [I#k_mb_incoming.customer_uuid || I <- Incomings,
                        I#k_mb_incoming.customer_uuid =/= undefined],
            Dict = k_storage_utils:get_uuid_to_customer_dict(Uuids),
            Resp = [build_resp(I, Dict) || I <- Incomings],
            {ok, Resp};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

build_resp(I, Dict) ->
    CustomerUuid = I#k_mb_incoming.customer_uuid,
    CustomerId =
        case CustomerUuid of
            undefined ->
                undefined;
            _ ->
                Customer = dict:fetch(CustomerUuid, Dict),
                Customer#customer.customer_id
        end,
    RcvTime = ac_datetime:timestamp_to_datetime(I#k_mb_incoming.rcv_time),
    [
        {msg_id, I#k_mb_incoming.id},
        {customer_uuid, CustomerUuid},
        {customer_id, CustomerId},
        {user_id, I#k_mb_incoming.user_id},
        {src_addr, k_storage_utils:addr_to_proplist(I#k_mb_incoming.src_addr)},
        {dst_addr, k_storage_utils:addr_to_proplist(I#k_mb_incoming.dst_addr)},
        {body, I#k_mb_incoming.body},
        {rcv_time, ac_datetime:datetime_to_iso8601(RcvTime)},
        {state, I#k_mb_incoming.state}
    ].
