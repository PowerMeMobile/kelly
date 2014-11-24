-module(k_delivery_status_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_sms_delivery_status_request_dto{}) ->
    {ok, #k1api_sms_delivery_status_response_dto{}} | {error, term()}.
process(ReqDTO) ->
    ReqId    = ReqDTO#k1api_sms_delivery_status_request_dto.id,
    SmsReqId = ReqDTO#k1api_sms_delivery_status_request_dto.sms_request_id,
    case shifted_storage:find(mt_messages, {ri, SmsReqId}) of
        {ok, Msgs} ->
            {ok, #k1api_sms_delivery_status_response_dto{
                id = ReqId,
                statuses = [status(Msg) || Msg <- Msgs]
            }};
        Error ->
            Error
    end.

%% ===================================================================
%% Interal
%% ===================================================================

status({_ID, MsgDoc}) ->
    Address = case bson:at(da, MsgDoc) of
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

timestamp(<<"pending">>, MsgDoc) ->
    bson:at(rqt, MsgDoc);
timestamp(<<"submitted">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
%% deprecated
timestamp(<<"sent">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
timestamp(<<"failed">>, MsgDoc) ->
    bson:at(rpt, MsgDoc);
timestamp(_, MsgDoc) ->
    bson:at(dt, MsgDoc).
