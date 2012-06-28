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
	{ok, DlrUpdates, ReceiptsData, NotProcessed} = traverse_receipt_batch(ReceiptBatch),
	ok = run_dlr_updates(DlrUpdates),
	ok = store_dlr_receipts(ReceiptsData),
	ok = store_not_processed(NotProcessed),
	{ok, []}.

traverse_receipt_batch(#'ReceiptBatch'{
	gatewayId = GatewayId,
	receipts = Receipts
}) ->
	DlrTime = k_storage_util:utc_unix_epoch(),
	{DlrUpdates, ReceiptsData, NotProcessed} = lists:foldl(fun(#'DeliveryReceipt'{
			messageId = MessageId,
			messageState = MessageState
		}, {AccDlrUpdates, AccFunReceipts, AccNotProcessed}) ->
			OutId = {GatewayId, MessageId},
			case k_storage_api:get_input_id_by_output_id(OutId) of
				{ok, InId = {CustomerId, InputMsgId}} ->
					?log_debug("[out:~p] -> [in:~p]", [OutId, InId]),
					case k_storage_api:get_msg_status(InId) of
						{ok, MsgStatus} ->
							case k_storage_api:get_msg_info(InId) of
								{ok, #msg_info{
										source_addr = SrcAddr,
										dest_addr = DstAddr
									}} ->
									DlrUpdate = {
										{CustomerId, InputMsgId},
										MsgStatus#msg_status{
											status = MessageState,
											dlr_time = DlrTime
										}
									},
									%% all data is ready, update the accumulators.
									{
										[DlrUpdate | AccDlrUpdates],
										[{CustomerId, InputMsgId, MessageState, SrcAddr, DstAddr, DlrTime} | AccFunReceipts],
										AccNotProcessed
									};
								{error, no_entry} ->
									?log_debug("No message info is stored yet for: ~p", [InId]),
									%% the message info was not yet stored, add it to not processed list.
									{
										AccDlrUpdates,
										AccFunReceipts,
										[{GatewayId, MessageId, MessageState, DlrTime} | AccNotProcessed]
									}
							end;
						{error, no_entry} ->
							?log_debug("No message status is stored yet for: ~p", [InId]),
							%% the message status was not yet stored, add it to not processed list.
							{
								AccDlrUpdates,
								AccFunReceipts,
								[{GatewayId, MessageId, MessageState, DlrTime} | AccNotProcessed]
							}
					end;
				{error, no_entry} ->
					?log_debug("No out to in mapping is stored yet for: ~p", [OutId]),
					%% this ids mapping was not yet stored, add it to not processed list.
					{
						AccDlrUpdates,
						AccFunReceipts,
						[{GatewayId, MessageId, MessageState, DlrTime} | AccNotProcessed]
					}
			end
		end,
		{[], [], []},
		Receipts),
	{ok, DlrUpdates, ReceiptsData, NotProcessed}.

run_dlr_updates([]) -> ok;
run_dlr_updates([{Id, Status} | SoFar]) ->
	ok = k_storage_api:set_msg_status(Id, Status),
	run_dlr_updates(SoFar).

store_dlr_receipts(ReceiptsData) ->
	{ok, ReceiptBatches} = k_funnel_asn_helper:render_receipts(ReceiptsData),
	lists:foreach(fun({CID, BatchId, BatchBinary}) ->
		% ?log_debug("Registering ReceiptBatch: ~p -> ~p", [CID, BatchBinary]),
		UserID = undefined,
		ok = k_mailbox:register_incoming_item(
			BatchId, CID, UserID, <<"ReceiptBatch">>, BatchBinary)
	end, ReceiptBatches),
	ok.

store_not_processed(NotProcessed) ->
	lists:foreach(
		fun({GatewayId, MessageId, Status, DlrTime}) ->
			OutId = {GatewayId, MessageId},
			MsgStatus = #msg_status{status = Status, dlr_time = DlrTime},
			k_storage_api:set_dlr_msg_status(OutId, MsgStatus)
		end, NotProcessed),
	?log_info("Stored [~p] not processed delivery statuses", [length(NotProcessed)]),
	ok.
