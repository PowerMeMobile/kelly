-module(k_incoming_sms_handler).

-export([process/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/JustAsn.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include("amqp_worker_reply.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"IncomingSm">>, Message) ->
	%?log_debug("Got message: ~p", [Message]),
	case 'JustAsn':decode('IncomingSm', Message) of
		{ok, IncomingSmsRequest} ->
			process_incoming_sms_request(IncomingSmsRequest);
		Error ->
			Error
	end;

process(CT, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
	{ok, []}.

process_incoming_sms_request(IncomingSmsRequest = #'IncomingSm'{
	gatewayId = GatewayId,
	source = SourceAddr,
	dest = DestAddr,
	message = MessageBody,
	dataCoding = DataCoding,
	partsRefNum = _PartsRefNum,
	partsCount = _PartsCount,
	partIndex = _PartIndex,
	timestamp = _UTCTime
}) ->
	?log_debug("Got request: ~p", [IncomingSmsRequest]),
	#'FullAddr'{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = DestAddr,
	%% generate new id.
	ItemId = k_uuid:to_string(k_uuid:newid()),
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
		case k_addr2cust:resolve(#addr{addr = Addr, ton = TON, npi = NPI}) of
			{ok, CustId, UserId} ->
				?log_debug("Got incoming message for [cust:~p; user: ~p] (addr:~p, ton:~p, npi:~p)",
					[CustId, UserId, Addr, TON, NPI] ),
				%% register it to futher sending.
				Batch = k_funnel_asn_helper:render_outgoing_batch(
					ItemId, SourceAddr, DestAddr, MessageBody, Encoding),
				k_mailbox:register_incoming_item(
					ItemId, CustId, UserId, <<"OutgoingBatch">>, Batch),
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
		body = iolist_to_binary(MessageBody),
		src_addr = transform_addr(SourceAddr),
		dst_addr = transform_addr(DestAddr),
		registered_delivery = false
	},
	?log_debug("~p", [MsgInfo]),
	%% determine receiving time.
	Time = k_datetime:utc_unix_epoch(),
	%% store it.
	store_incoming_msg_info(OutputId, MsgInfo, Time),
	?log_debug("Incoming message stored: out:~p", [OutputId]),
	{ok, []}.

transform_addr(#'FullAddr'{
	addr = Addr,
	ton = Ton,
	npi = Npi
}) ->
	#full_addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	};
transform_addr(#'FullAddrAndRefNum'{
	fullAddr = FullAddr,
	refNum = RefNum
}) ->
	#full_addr_ref_num{
		full_addr = transform_addr(FullAddr),
		ref_num = RefNum
	}.

-spec store_incoming_msg_info(msg_id(), #msg_info{}, integer()) -> ok.
store_incoming_msg_info(OutputId, MsgInfo, Time) ->
	ok = k_storage:set_incoming_msg_info(OutputId, MsgInfo),
	ok = k_statistic:store_incoming_msg_stats(OutputId, MsgInfo, Time).
