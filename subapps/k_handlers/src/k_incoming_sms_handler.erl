-module(k_incoming_sms_handler).

-export([process/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include_lib("k_mailbox/include/pending_item.hrl").
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
	timestamp = _UTCTime }) ->
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
	   -1 -> {text, default};
		0 -> {text, gsm0338};
		1 -> {text, ascii};
		3 -> {text, latin1};
		8 -> {text, ucs2};
		_ -> {other, DataCoding}
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
				Batch = build_funnel_incoming_message_dto(
					ItemId, SourceAddrDTO, DestAddrDTO, MessageBody, Encoding),
				Item = #k_mb_pending_item{
					item_id = ItemId,
					customer_id = CustId,
					user_id = UserId,
					content_type = <<"OutgoingBatch">>,
					sender_addr = SourceAddr,
					dest_addr = DestAddr,
					timestamp = k_datetime:utc_unix_epoch(),
					message_body = MessageBody,
					content_body = Batch,
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
	%% build msg_info out of available data
	MsgInfo = #msg_info{
		id = ItemId,
		gateway_id = GatewayId,
		customer_id = CustomerId,
		type = regular,
		encoding = Encoding,
		body = MessageBody,
		src_addr = transform_addr(SourceAddrDTO),
		dst_addr = transform_addr(DestAddrDTO),
		registered_delivery = false
	},
	%% determine receiving time.
	Time = k_datetime:utc_unix_epoch(),
	%% store it.
	store_incoming_msg_info(OutputId, MsgInfo, Time),
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

-spec store_incoming_msg_info(msg_id(), #msg_info{}, integer()) -> ok.
store_incoming_msg_info(OutputId, MsgInfo, Time) ->
	ok = k_storage:set_incoming_msg_info(OutputId, MsgInfo),
	ok = k_statistic:store_incoming_msg_stats(OutputId, MsgInfo, Time).

build_funnel_incoming_message_dto(BatchId, SrcAddr, DstAddr, MessageBody, DataCoding) ->
	Msg = #funnel_incoming_sms_message_dto{
		source = SrcAddr,
		dest = DstAddr,
		data_coding = DataCoding,
		message = MessageBody
	},
	Batch = #funnel_incoming_sms_dto{
		id = BatchId,
		messages = [Msg]
	},
	{ok, Binary} = adto:encode(Batch),
	Binary.
