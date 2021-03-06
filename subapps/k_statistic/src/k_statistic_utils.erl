-module(k_statistic_utils).

-export([
    get_timestamp_ranges/3,
    build_mt_msg_resp/3,
    build_mo_msg_resp/3
]).

-include("application.hrl").
-include_lib("k_storage/include/msg_info.hrl").

-type unixepoch() :: pos_integer().
-type plist() :: [{atom(), term()}].

%% ===================================================================
%% API
%% ===================================================================

-spec get_timestamp_ranges(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) -> [{unixepoch(), unixepoch()}].
get_timestamp_ranges(From, To, Step) when From < To ->
    Timestamps = get_timestamp_list(From, To, Step),
    ac_lists:make_ranges(Timestamps).

-spec build_mt_msg_resp(#msg_info{}, dict(), dict()) -> plist().
build_mt_msg_resp(MsgInfo, CustDict, GtwDict) ->
    CustomerUuid = MsgInfo#msg_info.customer_uuid,
    {CustomerId, CustomerName} =
        case dict:find(CustomerUuid, CustDict) of
            {ok, C} ->
                {C#customer.customer_id, C#customer.name};
            error ->
                {undefined, undefined}
        end,
    GatewayId = MsgInfo#msg_info.gateway_id,
    GatewayName =
        case dict:find(GatewayId, GtwDict) of
            {ok, G} ->
                G#gateway.name;
            error ->
                undefined
        end,
    Type = get_type(MsgInfo#msg_info.type),
    PartInfo = get_part_info(MsgInfo#msg_info.type),
    ReqTime  = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
    RespTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.resp_time),
    DlrTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.dlr_time),
    StatusTime = max(ReqTime, max(RespTime, DlrTime)),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    StatusISO = ac_datetime:datetime_to_iso8601(StatusTime),
    [
        {msg_id, MsgInfo#msg_info.msg_id},
        {client_type, MsgInfo#msg_info.client_type},
        {customer_uuid, CustomerUuid},
        {customer_id, CustomerId},
        {customer_name, CustomerName},
        {user_id, MsgInfo#msg_info.user_id},
        {in_msg_id, MsgInfo#msg_info.in_msg_id},
        {gateway_id, GatewayId},
        {gateway_name, GatewayName},
        {out_msg_id, MsgInfo#msg_info.out_msg_id},
        {type, Type},
        {part_info, PartInfo},
        {encoding, MsgInfo#msg_info.encoding},
        {body, MsgInfo#msg_info.body},
        {src_addr, k_storage_utils:addr_to_proplist(MsgInfo#msg_info.src_addr)},
        {dst_addr, k_storage_utils:addr_to_proplist(MsgInfo#msg_info.dst_addr)},
        {reg_dlr, MsgInfo#msg_info.reg_dlr},
        {esm_class, MsgInfo#msg_info.esm_class},
        {validity_period, MsgInfo#msg_info.val_period},
        {req_time, ReqISO},
        {status, MsgInfo#msg_info.status},
        {status_update_time, StatusISO},
        {network_id, MsgInfo#msg_info.network_id},
        {revenue, MsgInfo#msg_info.price}
    ].

-spec build_mo_msg_resp(#msg_info{}, dict(), dict()) -> plist().
build_mo_msg_resp(MsgInfo, CustDict, GtwDict) ->
    MsgId = MsgInfo#msg_info.msg_id,
    CustomerUuid = MsgInfo#msg_info.customer_uuid,
    {CustomerId, CustomerName} =
        case dict:find(CustomerUuid, CustDict) of
            {ok, C} ->
                {C#customer.customer_id, C#customer.name};
            error ->
                {undefined, undefined}
        end,
    GatewayId = MsgInfo#msg_info.gateway_id,
    GatewayName =
        case dict:find(GatewayId, GtwDict) of
            {ok, G} ->
                G#gateway.name;
            error ->
                undefined
        end,
    ReqTime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
    ReqISO = ac_datetime:datetime_to_iso8601(ReqTime),
    StatusISO = ReqISO,
    [
        {msg_id, MsgId},
        {customer_uuid, CustomerUuid},
        {user_id, MsgInfo#msg_info.user_id},
        {customer_id, CustomerId},
        {customer_name, CustomerName},
        {gateway_id, GatewayId},
        {gateway_name, GatewayName},
        {type, MsgInfo#msg_info.type},
        {encoding, MsgInfo#msg_info.encoding},
        {body, MsgInfo#msg_info.body},
        {src_addr, k_storage_utils:addr_to_proplist(MsgInfo#msg_info.src_addr)},
        {dst_addr, k_storage_utils:addr_to_proplist(MsgInfo#msg_info.dst_addr)},
        {reg_dlr, MsgInfo#msg_info.reg_dlr},
        {req_time, ReqISO},
        {status, MsgInfo#msg_info.status},

        {client_type, undefined},
        {in_msg_id, undefined},
        {out_msg_id, undefined},
        {part_info, undefined},
        {esm_class, 0},
        {validity_period, undefined},
        {status_update_time, StatusISO},
        {network_id, undefined},
        {revenue, 0.0}
    ].

%% ===================================================================
%% Internal
%% ===================================================================

-spec stats_report_frequency() -> integer().
stats_report_frequency() ->
    60.

-spec get_timestamp_list(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) ->
    [unixepoch()].
get_timestamp_list(From, To, Step) when From < To ->
    {FromFloor, ToCeiling} = align_time_range(From, To),
    List = lists:seq(FromFloor, ToCeiling, Step),
    case lists:last(List) < To of
        true -> List ++ [To];
        false -> List
    end.

-spec align_time_range(From::unixepoch(), To::unixepoch()) ->
    {FromFloor::unixepoch(), ToCeiling::unixepoch()}.
align_time_range(From, To) ->
    Step = stats_report_frequency(),
    align_time_range(From, To, Step).

-spec align_time_range(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) ->
    {FromFloor::unixepoch(), ToCeiling::unixepoch()}.
align_time_range(From, To, Step) ->
    FromFloor = From - From rem Step,
    ToCeiling = case To rem Step of
                    0 -> To;
                    Rem -> To - Rem + Step
                end,
    {FromFloor, ToCeiling}.

get_type(regular) ->
    regular;
get_type({part, #part_info{}}) ->
    part.

get_part_info(regular) ->
    undefined;
get_part_info({part, #part_info{
    ref = PartRef,
    seq = PartSeq,
    total = TotalParts
}}) -> [
    {ref, PartRef},
    {seq, PartSeq},
    {total, TotalParts}
].
