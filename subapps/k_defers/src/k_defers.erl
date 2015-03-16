-module(k_defers).

-export([
    get_all/1,
    get_details/1,
    get_recipients/1,
    update/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all(plist()) -> {ok, plist()} | {error, reason()}.
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

-spec get_details(uuid()) -> {ok, plist()} | {error, reason()}.
get_details(ReqId) ->
    case k_storage_defers:get_details(ReqId) of
        {ok, Batch} ->
            Resp = build_mt_batch_response(Batch),
            {ok, Resp};
        {error, Error} ->
            {error, Error}
    end.

-spec get_recipients(uuid()) -> {ok, plist()} | {error, reason()}.
get_recipients(ReqId) ->
    case k_storage_defers:get_recipients(ReqId) of
        {ok, Addrs} ->
            {ok, [k_storage_utils:addr_to_proplist(A) || A <- Addrs]};
        {error, Error} ->
            {error, Error}
    end.

-spec update(plist()) -> ok | {error, reason()}.
update(Params) ->
    ReqId = ?gv(req_id, Params),
    DefTime = datetime_to_timestamp(?gv(def_time, Params)),
    Body = ?gv(body, Params),
    check_and_maybe_update(ReqId, DefTime, Body).

%% ===================================================================
%% Internal
%% ===================================================================

build_mt_batch_response(Batch) ->
    CustomerUuid = Batch#batch_info.customer_uuid,
    CustomerId = CustomerUuid,
    DefTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.def_time),
    DefISO = ac_datetime:datetime_to_iso8601(DefTime),
    ReqTime = ac_datetime:timestamp_to_datetime(Batch#batch_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    [
        {req_id, Batch#batch_info.req_id},
        {customer_uuid, CustomerUuid},
        {customer_id, CustomerId},
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
        {revenue, Batch#batch_info.price}
    ].

datetime_to_timestamp(undefined) ->
    undefined;
datetime_to_timestamp({{_,_,_},{_,_,_}} = Datetime) ->
    ac_datetime:datetime_to_timestamp(Datetime).

check_and_maybe_update(_ReqId, undefined, undefined) ->
    ok;
check_and_maybe_update(ReqId, DefTime, Body) ->
    check_def_time(ReqId, DefTime, Body).

check_def_time(ReqId, DefTime, Body) ->
    case DefTime of
        undefined ->
            check_req_id(ReqId, DefTime, Body);
        _ ->
            case DefTime >= ac_datetime:utc_timestamp() of
                true ->
                    check_req_id(ReqId, DefTime, Body);
                false ->
                    {error, def_time_in_past}
            end
    end.

check_req_id(ReqId, DefTime, Body) ->
    case k_storage_defers:get_details(ReqId) of
        {ok, Batch} ->
            check_req_type(ReqId, DefTime, Body, Batch);
        {error, no_entry} ->
            {error, no_entry}
    end.

check_req_type(ReqId, DefTime, Body, Batch) ->
    ReqType = Batch#batch_info.req_type,
    case {ReqType, Body} of
        {single, undefined} ->
            k_storage_defers:update(ReqId, DefTime, Body);
        {single, _Body} ->
            check_body(ReqId, DefTime, Body, Batch);
        {multiple, undefined} ->
            k_storage_defers:update(ReqId, DefTime, Body);
        {multiple, _Body} ->
            {error, req_type_multiple}
    end.

check_body(ReqId, DefTime, Body, Batch) ->
    BatchEnc = Batch#batch_info.encoding,
    Recipients = Batch#batch_info.recipients,
    Messages = Batch#batch_info.messages,
    BatchParts = round(Messages/Recipients),

    {ok, GuessEnc} = alley_services_utils:guess_encoding(Body),
    GuessSize = alley_services_utils:chars_size(GuessEnc, Body),
    GuessParts = alley_services_utils:calc_parts_number(GuessSize, GuessEnc),

    case BatchEnc =:= GuessEnc of
        true ->
            case BatchParts =:= GuessParts of
                true ->
                    k_storage_defers:update(ReqId, DefTime, Body);
                false ->
                    {error, {body_parts, BatchParts, GuessParts}}
            end;
        false ->
            {error, {body_encoding, BatchEnc, GuessEnc}}
    end.
