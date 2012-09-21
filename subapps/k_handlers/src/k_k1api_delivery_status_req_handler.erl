-module(k_k1api_delivery_status_req_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#k1api_sms_delivery_status_request_dto{}, Message) of
		{ok, Request} ->
			process_delivery_status_request(Request);
		Error ->
			?log_error("k1api dto decode error: ~p", [Error]),
			{ok, []}
	end.

%% ===================================================================
%% Interal
%% ===================================================================

process_delivery_status_request(Request) ->
	?log_debug("Got k1api sms delivery status request: ~p", [Request]),
	#k1api_sms_delivery_status_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = _UserID,
		sms_request_id = SmsRequestID,
		address = SenderAddress
	} = Request,
	UserID = undefined,
	Key = {CustomerID, UserID, SenderAddress, SmsRequestID},
	case k_storage:get_msg_ids_by_sms_request_id(Key) of
		{ok, IDs} ->
			get_statuses(RequestID, IDs);
		{error, Error} ->
			?log_error("k_storage unexpected error: ~p", [Error]),
			{ok, []}
	end.

get_statuses(RequestID, InputIDs) ->

	%% get status & destination adress for each Input message id
	%% and put it into dto record #k1api_sms_status_dto
	StatusesDTO = lists:map(fun(InputID) ->
		{ok, MessageInfo} = k_storage:get_msg_info(InputID),
		AddressDTO = convert_addr(MessageInfo#msg_info.dst_addr),
		{ok, MessageStatus} = k_storage:get_msg_status(InputID),
		Status = MessageStatus#msg_status.status,
		#k1api_sms_status_dto{
			address = AddressDTO,
			status = Status
		}
	end, InputIDs),

	DTO = #k1api_sms_delivery_status_response_dto{
		id = RequestID,
		statuses = StatusesDTO
	},
	reply(DTO).

reply(DTO) ->
	case adto:encode(DTO) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.k1api.delivery_status_response">>,
				content_type = <<"OneAPIDeliveryStatusResponse">>,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api dto encode error: ~p", [Error]),
	   		{ok, []}
	end.

convert_addr(FullAddr = #full_addr{}) ->
	#full_addr{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = FullAddr,
	#addr_dto{
		addr = Addr,
		ton = TON,
		npi = NPI
	};
convert_addr(FullAddrAndRefNum = #full_addr_ref_num{}) ->
	#full_addr_ref_num{
		full_addr = FullAddr
	} = FullAddrAndRefNum,
	convert_addr(FullAddr).
