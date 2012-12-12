-module(k_incoming_sms_handler).

-export([process/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include_lib("k_mailbox/include/application.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include("amqp_worker_reply.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"IncomingSm">>, Message) ->
	?log_debug("Got just incoming sms request", []),
	case adto:decode(#just_incoming_sms_dto{}, Message) of
		{ok, IncomingSmsRequest} ->
			process_incoming_sms_request(IncomingSmsRequest);
		Error ->
			Error
	end;

process(CT, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
	{ok, []}.

process_incoming_sms_request(#just_incoming_sms_dto{
	gateway_id = GatewayId,
	source = SourceAddrDTO,
	dest = DestAddrDTO,
	message = MessageBody,
	data_coding = DataCoding,
	parts_ref_num = _PartsRefNum,
	parts_count = _PartsCount,
	part_index = _PartIndex,
	timestamp = _UTCTime
}) ->
	#addr_dto{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = DestAddrDTO,
	DestAddr = #addr{addr = Addr, ton = TON, npi = NPI},
	SourceAddr = #addr{
		addr = SourceAddrDTO#addr_dto.addr,
		ton = SourceAddrDTO#addr_dto.ton,
		npi = SourceAddrDTO#addr_dto.npi
	},
	%% generate new id.
	ItemId = uuid:newid(),
	%% transform encoding.
	Encoding = case DataCoding of
	   -1 -> default;
		0 -> gsm0338;
		1 -> ascii;
		3 -> latin1;
		8 -> ucs2;
		_ -> DataCoding
	end,
	%% try to determine customer id and user id,
	%% this will return either valid customer id or `undefined'.
	%% i think it makes sense to store even partly filled message.
	CustomerId =
		case k_addr2cust:resolve(DestAddr) of
			{ok, CustId, UserId} ->
				?log_debug("Got incoming message for [cust:~p; user: ~p] (addr:~p, ton:~p, npi:~p)",
					[CustId, UserId, Addr, TON, NPI] ),

				%% register it to futher sending.
				Item = #k_mb_incoming_sms{
					id = ItemId,
					customer_id	= CustId,
					user_id	= UserId,
					source_addr	= SourceAddr,
					dest_addr = DestAddr,
					received  = k_datetime:utc_timestamp(),
					message_body = MessageBody,
					encoding = Encoding
				},
				k_mailbox:register_incoming_item(Item),
				?log_debug("Incomming message registered [item:~p]", [ItemId]),
				%% return valid customer.
				CustId;
	    Error ->
			?log_debug("Address resolution failed with: ~p", [Error]),
			?log_debug("Could not resolve incoming message to (addr:~p, ton:~p, npi:~p)", [Addr, TON, NPI]),
			%% return `undefined' customer.
			undefined
	end,
	%% build OutputId.
	OutputId = {GatewayId, ItemId},
	%% build msg_info out of available data.
	MsgInfo = #msg_info{
		in_msg_id = ItemId,
		gateway_id = GatewayId,
		customer_id = CustomerId,
		type = regular,
		encoding = Encoding,
		body = MessageBody,
		src_addr = transform_addr(SourceAddrDTO),
		dst_addr = transform_addr(DestAddrDTO),
		reg_dlr = false,
		req_time = k_datetime:utc_timestamp()
	},
	%% store it.
	ok = k_storage:set_incoming_msg_info(MsgInfo),
	?log_debug("Incoming message stored: out:~p", [OutputId]),
	{ok, []}.

transform_addr(#addr_dto{
	addr = Addr,
	ton = Ton,
	npi = Npi
}) ->
	#full_addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	};
transform_addr(#addr_ref_num_dto{
	full_addr = FullAddr,
	ref_num = RefNum
}) ->
	#full_addr_ref_num{
		full_addr = transform_addr(FullAddr),
		ref_num = RefNum
	}.
