-module(k_statistic_utils).

-export([
    get_timestamp_ranges/3,
    doc_to_mt_msg/1,
    doc_to_mo_msg/1
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

-spec doc_to_mt_msg(bson:document()) -> plist().
doc_to_mt_msg(Doc) ->
    MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
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
        %% customer_id is deprecated, force clients to use customer_uuid
        {customer_id, MsgInfo#msg_info.customer_id},
        {customer_uuid, MsgInfo#msg_info.customer_id},
        {user_id, MsgInfo#msg_info.user_id},
        {in_msg_id, MsgInfo#msg_info.in_msg_id},
        {gateway_id, MsgInfo#msg_info.gateway_id},
        {out_msg_id, MsgInfo#msg_info.out_msg_id},
        {type, Type},
        {part_info, PartInfo},
        {encoding, MsgInfo#msg_info.encoding},
        {body, MsgInfo#msg_info.body},
        {src_addr, addr_to_proplist(MsgInfo#msg_info.src_addr)},
        {dst_addr, addr_to_proplist(MsgInfo#msg_info.dst_addr)},
        {reg_dlr, MsgInfo#msg_info.reg_dlr},
        {esm_class, MsgInfo#msg_info.esm_class},
        {validity_period, MsgInfo#msg_info.val_period},
        {req_time, ReqISO},
        {status, MsgInfo#msg_info.status},
        {status_update_time, StatusISO},
        {network_id, MsgInfo#msg_info.network_id},
        {revenue, MsgInfo#msg_info.price}
    ].

-spec doc_to_mo_msg(bson:document()) -> plist().
doc_to_mo_msg(Doc) ->
    MsgInfo = k_storage_utils:doc_to_mo_msg_info(Doc),
    MsgId = MsgInfo#msg_info.msg_id,
    Datetime = ac_datetime:timestamp_to_datetime(MsgInfo#msg_info.req_time),
    ISO8601 = ac_datetime:datetime_to_iso8601(Datetime),
    [
        {msg_id, MsgId},
        %% customer_id is deprecated, force clients to use customer_uuid
        {customer_id, MsgInfo#msg_info.customer_id},
        {customer_uuid, MsgInfo#msg_info.customer_id},
        {in_msg_id, MsgInfo#msg_info.in_msg_id},
        {gateway_id, MsgInfo#msg_info.gateway_id},
        {out_msg_id, MsgInfo#msg_info.out_msg_id},
        {type, MsgInfo#msg_info.type},
        {encoding, MsgInfo#msg_info.encoding},
        {body, MsgInfo#msg_info.body},
        {src_addr, addr_to_proplist(MsgInfo#msg_info.src_addr)},
        {dst_addr, addr_to_proplist(MsgInfo#msg_info.dst_addr)},
        {reg_dlr, MsgInfo#msg_info.reg_dlr},
        {req_time, ISO8601}
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

addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi}
    ];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi},
        {ref_num, RefNum}
    ].

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
