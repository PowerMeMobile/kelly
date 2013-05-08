-module(k_incoming_sms_handler).

-export([process/2]).

-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/address.hrl").
-include_lib("k_mailbox/include/application.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include("amqp_worker_reply.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"IncomingSm">>, Message) ->
	case adto:decode(#just_incoming_sms_dto{}, Message) of
		{ok, IncomingSmsRequest} ->
			process_incoming_sms_request(IncomingSmsRequest);
		Error ->
			Error
	end;

process(CT, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
	{ok, []}.

process_incoming_sms_request(IncSmsRequest = #just_incoming_sms_dto{
	gateway_id = GatewayId,
	source = SourceAddr,
	dest = DestAddr,
	message = MessageBody,
	data_coding = DataCoding,
	parts_ref_num = _PartsRefNum,
	parts_count = _PartsCount,
	part_index = _PartIndex,
	timestamp = UTCString
}) ->
	?log_debug("Got just incoming sms request:~p ", [IncSmsRequest]),

	%% generate new id.
	ItemId = uuid:unparse(uuid:generate_time()),

	%% determine
	Timestamp = k_datetime:utc_string_to_timestamp(UTCString),

	%% try to determine customer id and user id,
	%% this will return either valid customer id or `undefined'.
	%% i think it makes sense to store even partly filled message.
	CustomerId =
		case k_addr2cust:resolve(DestAddr) of
			{ok, CustId, UserId} ->
				?log_debug("Got incoming message for [cust:~p; user: ~p] (~p)",
					[CustId, UserId, DestAddr] ),

				%% register it to futher sending.
				Item = #k_mb_incoming_sms{
					id = ItemId,
					customer_id	= CustId,
					user_id	= UserId,
					source_addr	= SourceAddr,
					dest_addr = DestAddr,
					received  = Timestamp,
					message_body = MessageBody,
					encoding = DataCoding
				},
				k_mailbox:register_incoming_item(Item),
				?log_debug("Incomming message registered [item:~p]", [ItemId]),
				%% return valid customer.
				CustId;
	    Error ->
			?log_debug("Address resolution failed with: ~p", [Error]),
			?log_debug("Could not resolve incoming message to ~p", [DestAddr]),
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
		encoding = DataCoding,
		body = MessageBody,
		src_addr = SourceAddr,
		dst_addr = DestAddr,
		reg_dlr = false,
		req_time = Timestamp
	},
	%% store it.
	ok = k_dynamic_storage:set_mo_msg_info(MsgInfo),
	?log_debug("Incoming message stored: out:~p", [OutputId]),
	{ok, []}.
