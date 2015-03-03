-module(k_delivery_status_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(record()) ->
    {ok, record()} | {error, term()}.
process(ReqDTO = #k1api_sms_delivery_status_request_dto{}) ->
    ReqId    = ReqDTO#k1api_sms_delivery_status_request_dto.id,
    SmsReqId = ReqDTO#k1api_sms_delivery_status_request_dto.sms_request_id,
    case shifted_storage:find(mt_messages, {'ri', SmsReqId}) of
        {ok, Msgs} ->
            {ok, #k1api_sms_delivery_status_response_dto{
                id = ReqId,
                statuses = [status_k1api(Msg) || Msg <- Msgs]
            }};
        Error ->
            Error
    end;
process(ReqDTO = #sms_status_req_v1{}) ->
    ReqId    = ReqDTO#sms_status_req_v1.req_id,
    SmsReqId = ReqDTO#sms_status_req_v1.sms_req_id,
    case shifted_storage:find(mt_messages, {'ri', SmsReqId},
            {'_id', 0, 'imi', 1, 'da', 1, 't', 1, 's', 1, 'rqt', 1, 'rpt', 1, 'dt', 1}) of
        {ok, MsgDocs} ->
            {ok, #sms_status_resp_v1{
                req_id = ReqId,
                statuses = statuses_v1(MsgDocs)
            }};
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================

status_k1api({_Id, MsgDoc}) ->
    Address =
        case bson:at(da, MsgDoc) of
        <<"xxxxxxxxxx">> ->
            %% the most probable case when this happens is
            %% when the sms request hasn't yet been processed,
            %% possibly due to an error in it.
            #addr{addr = <<"unknown">>, ton = 5, npi = 0};
        AddrDoc ->
            k_storage_utils:doc_to_addr(AddrDoc)
    end,
    Status = bson:at(s, MsgDoc),
    #k1api_sms_status_dto{
        address = Address,
        status = Status,
        timestamp = ac_datetime:timestamp_to_unixepoch(
            timestamp(Status, MsgDoc))
    }.

statuses_v1(MsgDocs) ->
    %% If a multipart message has different part statuses,
    %% then compress them to one status, i.e.
    %% [delivered, blocked] -> blocked
    %% [pending, submitted] -> pending
    compress_statuses_v1(addr_to_statuses_map(MsgDocs)).

compress_statuses_v1(Addr2Sts) ->
    compress_statuses_v1(Addr2Sts, []).

compress_statuses_v1([], Acc) ->
    Acc;
compress_statuses_v1([{Addr, [{St, Ts}]} | Addr2Sts], Acc) ->
    Status = #sms_status_v1{
        address = Addr,
        status = St,
        timestamp = ac_datetime:timestamp_to_unixepoch(Ts)
    },
    compress_statuses_v1(Addr2Sts, [Status | Acc]);
compress_statuses_v1([{Addr, StTss} | Addr2Sts], Acc) ->
    {St, Ts} = choose_status(StTss),
    Status = #sms_status_v1{
        address = Addr,
        status = St,
        timestamp = ac_datetime:timestamp_to_unixepoch(Ts)
    },
    compress_statuses_v1(Addr2Sts, [Status | Acc]).

choose_status([StTs | StTss]) ->
    choose_status(StTs, StTss).

choose_status({St, Ts}, []) ->
    {St, Ts};
choose_status({St, Ts}, [{St2, Ts2} | StTss]) ->
    case priority(St) < priority(St2) of
        true ->
            choose_status({St2, max(Ts, Ts2)}, StTss);
        false ->
            choose_status({St, max(Ts, Ts2)}, StTss)
    end.

priority(<<"delivered">>) -> 0;
priority(<<"submitted">>) -> 2;
priority(<<"failed">>)    -> 5;
priority(<<"blocked">>)   -> 4;
priority(<<"pending">>)   -> 3;
priority(_) -> 1.

addr_to_statuses_map(MsgDocs) ->
    addr_to_statuses_map(MsgDocs, dict:new()).

addr_to_statuses_map([], Dict) ->
    dict:to_list(Dict);
addr_to_statuses_map([{_Id, MsgDoc} | MsgDocs], Dict) ->
    case {bson:at(da, MsgDoc), bson:at(t, MsgDoc)} of
        {<<"xxxxxxxxxx">>, _} ->
            addr_to_statuses_map(MsgDocs, Dict);
        {AddrDoc, <<"regular">>} ->
            RefNum = bson:at(imi, MsgDoc),
            AddrDoc2 = bson:merge({r, RefNum}, AddrDoc),
            addr_to_statuses_map(MsgDocs, to_dict(AddrDoc2, MsgDoc, Dict));
        {AddrDoc, PartDoc} ->
            RefNum = ref_num(PartDoc),
            AddrDoc2 = bson:merge({r, RefNum}, AddrDoc),
            addr_to_statuses_map(MsgDocs, to_dict(AddrDoc2, MsgDoc, Dict))
    end.

to_dict(AddrDoc, MsgDoc, Dict) ->
    Addr = k_storage_utils:doc_to_addr(AddrDoc),
    Status = bson:at(s, MsgDoc),
    Ts = timestamp(Status, MsgDoc),
    %Ts = ac_datetime:timestamp_to_unixepoch(timestamp(Status, MsgDoc)),
    ac_dict:prepend(Addr, {Status, Ts}, Dict).

ref_num(Part) ->
    [Ref | _] = bson:at(r, Part),
    Ref.

timestamp(<<"pending">>, MsgDoc) ->
    bson:at(rqt, MsgDoc);
timestamp(<<"submitted">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
%% deprecated
timestamp(<<"sent">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
timestamp(<<"failed">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
timestamp(<<"blocked">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
timestamp(_, MsgDoc) ->
    bson:at(dt, MsgDoc).
