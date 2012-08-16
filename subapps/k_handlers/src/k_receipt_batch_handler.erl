-module(k_receipt_batch_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/JustAsn.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ReceiptBatch">>, Message) ->
	%?log_debug("Got message...", []),
	case 'JustAsn':decode('ReceiptBatch', Message) of
		{ok, ReceiptBatch} ->
			process_receipt_batch(ReceiptBatch);
		Error ->
			Error
	end;

process(CT, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
	{ok, []}.

-spec process_receipt_batch(#'ReceiptBatch'{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_receipt_batch(ReceiptBatch = #'ReceiptBatch'{
	gatewayId = GatewayIdStr,
	receipts = Receipts
}) ->
	GatewayId = k_uuid:to_binary(GatewayIdStr),
	?log_debug("Got request: ~p", [ReceiptBatch]),
	DlrTime = k_datetime:utc_unix_epoch(),
	case traverse_delivery_receipts(GatewayId, DlrTime, Receipts) of
		ok ->
			{ok, []};
		%% abnormal case, either sms request or response isn't handled yet, or both.
		{error, no_entry} ->
			{error, not_enough_data_to_proceed};
		Error ->
			Error
	end.

traverse_delivery_receipts(_GatewayId, _DlrTime, []) ->
	ok;
traverse_delivery_receipts(GatewayId, DlrTime,
	[#'DeliveryReceipt'{messageId = MessageIdStr, messageState = MessageState} | Receipts]) ->
	MessageId = list_to_binary(MessageIdStr),
	OutputId = {GatewayId, MessageId},
	case k_storage:get_input_id_by_output_id(OutputId) of
		{ok, InputId} ->
			?log_debug("[out:~p] -> [in:~p]", [OutputId, InputId]),
			case k_storage:get_msg_info(InputId) of
				{ok, MsgInfo} ->
					case update_delivery_state(InputId, OutputId, MsgInfo, DlrTime, MessageState) of
						ok ->
							case register_funnel_delivery_receipt(InputId, MsgInfo, DlrTime, MessageState) of
								ok ->
									traverse_delivery_receipts(GatewayId, DlrTime, Receipts);
								Error ->
									Error
							end;
						Error ->
							Error
					end;
				Error ->
					Error
			end;
		Error ->
			Error
	end.

update_delivery_state(InputId, OutputId, MsgInfo, DlrTime, MessageState) ->
	case k_storage:get_msg_status(InputId) of
		{ok, MsgStatus} ->
			NewMsgStatus = MsgStatus#msg_status{
				status = MessageState,
				dlr_time = DlrTime
			},
			ok = k_storage:set_msg_status(InputId, NewMsgStatus),
			ok = k_statistic:store_status_stats(InputId, OutputId, MsgInfo, NewMsgStatus, DlrTime);
		Error ->
			Error
	end.

register_funnel_delivery_receipt(InputId, MsgInfo, DlrTime, MessageState) ->
	SrcAddr = MsgInfo#msg_info.src_addr,
	DstAddr = MsgInfo#msg_info.dst_addr,
	Data = {InputId, MessageState, SrcAddr, DstAddr, DlrTime},
	{CustomerId, BatchId, BatchBinary} = k_funnel_asn_helper:render_receipt(Data),
	UserId = <<"undefined">>,
	ok = k_mailbox:register_incoming_item(
		k_uuid:to_binary(BatchId), CustomerId, UserId, <<"ReceiptBatch">>, BatchBinary
	).
