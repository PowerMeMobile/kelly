-module(k_receipt_batch_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ReceiptBatch">>, Message) ->
	% ?log_debug("Got message...", []),
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
process_receipt_batch(ReceiptBatch = #'ReceiptBatch'{}) ->
	?log_debug("Got request: ~p", [ReceiptBatch]),
	{ok, DlrUpdates, ReceiptsData} = traverse_receipt_batch(ReceiptBatch),
	ok = run_dlr_updates(DlrUpdates),
	ok = store_dlr_receipts(ReceiptsData),
	{ok, []}.

traverse_receipt_batch(#'ReceiptBatch'{
	gatewayId = GatewayId,
	receipts = Receipts
}) ->
	DlrTime = k_storage_util:utc_unix_epoch(),
	{DlrUpdates, ReceiptsData} = lists:foldl(fun(#'DeliveryReceipt'{
			messageId = MessageId,
			messageState = MessageState
		}, {AccDlrUpdates, AccFunReceipts}) ->
			OutputId = {GatewayId, MessageId},
			case k_storage_api:get_input_id_by_output_id(OutputId) of
				{ok, InputId = {CustomerId, InputMsgId}} ->
					?log_debug("[out:~p] -> [in:~p]", [OutputId, InputId]),
					case k_storage_api:get_msg_status(InputId) of
						{ok, MsgStatus} ->
							case k_storage_api:get_msg_info(InputId) of
								{ok, MsgInfo = #msg_info{
										source_addr = SrcAddr,
										dest_addr = DstAddr
									}} ->
									DlrUpdate = {
										InputId,
										OutputId,
										MsgInfo,
										MsgStatus#msg_status{
											status = MessageState,
											dlr_time = DlrTime
										}
									},
									%% all data is ready, update the accumulators.
									{
										[DlrUpdate | AccDlrUpdates],
										[{CustomerId, InputMsgId, MessageState, SrcAddr, DstAddr, DlrTime} | AccFunReceipts]
									};
								Error ->
									Error
							end;
						Error ->
							Error
					end;
				Error ->
					Error
			end
		end,
		{[], []},
		Receipts),
	{ok, DlrUpdates, ReceiptsData}.

run_dlr_updates([]) -> ok;
run_dlr_updates([{InputId, OutputId, MsgInfo, MsgStatus = #msg_status{status = Status, dlr_time = DlrTime}} | Rest]) ->
	ok = k_storage_api:set_msg_status(InputId, MsgStatus),
	ok = k_reports_api:store_status_stats(InputId, OutputId, MsgInfo, Status, DlrTime),
	run_dlr_updates(Rest).

store_dlr_receipts(ReceiptsData) ->
	{ok, ReceiptBatches} = k_funnel_asn_helper:render_receipts(ReceiptsData),
	lists:foreach(fun({CID, BatchId, BatchBinary}) ->
		% ?log_debug("Registering ReceiptBatch: ~p -> ~p", [CID, BatchBinary]),
		UserID = undefined,
		ok = k_mailbox:register_incoming_item(
			BatchId, CID, UserID, <<"ReceiptBatch">>, BatchBinary)
	end, ReceiptBatches),
	ok.
