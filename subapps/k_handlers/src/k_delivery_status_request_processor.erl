-module(k_delivery_status_request_processor).

-export([process/1]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(#k1api_sms_delivery_status_request_dto{}) ->
    {ok, #k1api_sms_delivery_status_response_dto{}} | {error, term()}.
process(DeliveryStatusReqDTO) ->
    ReqId    = DeliveryStatusReqDTO#k1api_sms_delivery_status_request_dto.id,
    SmsReqId = DeliveryStatusReqDTO#k1api_sms_delivery_status_request_dto.sms_request_id,
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
    #k1api_sms_status_dto{
        address = k_storage_utils:doc_to_addr(bson:at(da, MsgDoc)),
        status = bson:at(s, MsgDoc)
    }.
