-module(k_receipt_batch_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/pending_item.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ReceiptBatch">>, Message) ->
	case adto:decode(#just_delivery_receipt_dto{}, Message) of
		{ok, ReceiptBatch} ->
			process_receipt_batch(ReceiptBatch);
		Error ->
			Error
	end;

process(ContentType, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [ContentType, Message]),
	{ok, []}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec process_receipt_batch(#just_delivery_receipt_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_receipt_batch(ReceiptBatch = #just_delivery_receipt_dto{
	gateway_id = GatewayId,
	receipts = Receipts }) ->
	?log_debug("Got just delivery receipt: ~p", [ReceiptBatch]),
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
	[#just_receipt_dto{message_id = MessageId, message_state = MessageState} | Receipts]) ->
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
	SenderAddr = MsgInfo#msg_info.src_addr,
	DestAddr = MsgInfo#msg_info.dst_addr,
	{CustomerId, _ClientType, InputMsgId} = InputId,
	UserId = <<"undefined">>,
	ItemId = uuid:newid(),
	Item = #k_mb_pending_item{
		item_id = ItemId,
		customer_id = CustomerId,
		user_id = UserId,
		content_type = <<"ReceiptBatch">>,
		sender_addr = conver_addr(SenderAddr),
		dest_addr = conver_addr(DestAddr),
		input_id = InputMsgId,
		submit_date = list_to_binary(unix_to_utc(DlrTime)),
		done_date = list_to_binary(unix_to_utc(DlrTime)),
		message_state = MessageState
	 },
	ok = k_mailbox:register_incoming_item(Item).


unix_to_utc(TS) ->
	NM = TS div 1000000,
	NS = TS - (NM * 1000000),
	T = {NM, NS, 0},
	{{YY, MM, DD}, {H, M, S}} = calendar:now_to_universal_time(T),
	lists:map(
		fun(C) ->
			case C of
				$\  -> $0;
				_ -> C
			end
		end,
		lists:flatten(io_lib:format("~4B~2B~2B~2B~2B~2B", [YY, MM, DD, H, M, S]))
	).


conver_addr(undefined) ->
	undefined;
conver_addr(Addr = #full_addr{}) ->
	#full_addr{
		addr = Msisdn,
		ton = TON,
		npi = NPI
	} = Addr,
	#addr{
		addr = Msisdn,
		ton = TON,
		npi = NPI
	};
conver_addr(Addrs) ->
	[conver_addr(Addr) || Addr <- Addrs].
