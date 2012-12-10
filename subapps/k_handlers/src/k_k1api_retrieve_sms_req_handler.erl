-module(k_k1api_retrieve_sms_req_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_mailbox/include/application.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#k1api_retrieve_sms_request_dto{}, Message) of
		{ok, Request} ->
			process_retrive_sms_request(Request);
		Error ->
			?log_error("k1api dto decode error: ~p", [Error]),
			{ok, []}
	end.

%% ===================================================================
%% Interal
%% ===================================================================

process_retrive_sms_request(Request) ->
	?log_debug("Got k1api retrieve sms request: ~p", [Request]),
	#k1api_retrieve_sms_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = _UserID,
		dest_addr = DestAddrDTO,
		batch_size = BatchSize
	} = Request,
	UserID = <<"undefined">>,
	DestAddr = #addr{
		addr = DestAddrDTO#addr_dto.addr,
		ton = DestAddrDTO#addr_dto.ton,
		npi = DestAddrDTO#addr_dto.npi
	},
	{ok, IncomingSms, Total} = k_mailbox:get_incoming_sms(CustomerID, UserID, DestAddr, BatchSize),
	build_response(RequestID, IncomingSms, Total).

build_response(RequestID, IncomingSms, Total) ->
	?log_debug("RequestID: ~p, IncomingSms: ~p, Total: ~p",
		[RequestID, IncomingSms, Total]),
	MessagesDTO = lists:map(fun(PendingItem) ->
	#k_mb_incoming_sms{
		id = ItemID,
		source_addr	= SourceAddr,
		received = Time,
		message_body = Message
	} = PendingItem,
		DestAddrDTO = #addr_dto{
			addr = SourceAddr#addr.addr,
			ton = SourceAddr#addr.ton,
			npi = SourceAddr#addr.npi
		},
		#k1api_retrieved_sms_dto{
			datetime = Time,
			sender_addr = DestAddrDTO,
			message_id = ItemID,
			message = Message
		}
	end, IncomingSms),
	DTO = #k1api_retrieve_sms_response_dto{
		id = RequestID,
		messages = MessagesDTO,
		total = Total
	},
	?log_debug("DTO: ~p", [DTO]),
	reply(DTO).

reply(DTO) ->
	case adto:encode(DTO) of
		{ok, Binary} ->
			Reply = #worker_reply{
				reply_to = <<"pmm.k1api.retrieve_sms_response">>,
				content_type = <<"OneAPIRetrievedSmsResponse">>,
				payload = Binary},
			{ok, [Reply]};
		Error ->
			?log_warn("Unexpected k1api dto encode error: ~p", [Error]),
	   		{ok, []}
	end.
