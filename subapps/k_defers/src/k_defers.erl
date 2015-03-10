-module(k_defers).

-export([
    get_all/1,
    get_details/1,
    get_recipients/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all([{atom(), term()}]) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_all(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    UserId = ?gv(user_id, Params),
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),
    case k_storage_defers:get_all(CustomerUuid, UserId, Skip, Limit) of
        {ok, Batches} ->
            {ok, [build_mt_batch_response(B) || B <- Batches]};
        {error, Error} ->
            {error, Error}
    end.

-spec get_details(uuid()) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_details(ReqId) ->
    case k_storage_defers:get_details(ReqId) of
        {ok, Batch} ->
            Resp = build_mt_batch_response(Batch),
            {ok, Resp};
        {error, Error} ->
            {error, Error}
    end.

-spec get_recipients(uuid()) -> {ok, [[{atom(), term()}]]} | {error, reason()}.
get_recipients(ReqId) ->
    case k_storage_defers:get_recipients(ReqId) of
        {ok, Addrs} ->
            {ok, [k_storage_utils:addr_to_proplist(A) || A <- Addrs]};
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% Internal
%% ===================================================================

build_mt_batch_response(Batch) ->
    DefTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.def_time),
    DefISO = ac_datetime:datetime_to_iso8601(DefTime),
    ReqTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    [
        {req_id, Batch#batch_info.req_id},
        {customer_uuid, Batch#batch_info.customer_id},
        {user_id, Batch#batch_info.user_id},
        {client_type, Batch#batch_info.client_type},
        {def_time, DefISO},
        {req_type, Batch#batch_info.req_type},
        {src_addr, k_storage_utils:addr_to_proplist(Batch#batch_info.src_addr)},
        {encoding, Batch#batch_info.encoding},
        {body, Batch#batch_info.body},
        {reg_dlr, Batch#batch_info.reg_dlr},
        {req_time, ReqISO},
        {recipients, Batch#batch_info.recipients},
        {messages, Batch#batch_info.messages},
        {price, Batch#batch_info.price}
    ].
