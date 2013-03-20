-module(k_receipt_batch_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/application.hrl").

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
	receipts = Receipts
}) ->
	?log_debug("Got just delivery receipt: ~p", [ReceiptBatch]),
	DlrTime = k_datetime:utc_timestamp(),
	case traverse_delivery_receipts(GatewayId, DlrTime, Receipts) of
		ok ->
			{ok, []};
		%% abnormal case, sms response isn't handled yet.
		{error, no_entry} ->
			{error, not_enough_data_to_proceed};
		Error ->
			Error
	end.

traverse_delivery_receipts(_GatewayId, _DlrTime, []) ->
	ok;
traverse_delivery_receipts(GatewayId, DlrTime,
	[#just_receipt_dto{message_id = OutMsgId, message_state = DlrStatus} | Receipts]) ->
	%% note that this is one-shot call. it tries to store #dlr_info{}
	%% and, if succeeds, it returns the whole document.
	DlrInfo = #dlr_info{
		gateway_id = GatewayId,
		out_msg_id = OutMsgId,
		dlr_time = DlrTime,
		dlr_status = DlrStatus
	},
	case k_dynamic_storage:set_mt_dlr_info_and_get_msg_info(DlrInfo) of
		{ok, MsgInfo} ->
			ok = register_delivery_receipt(MsgInfo, DlrTime, DlrStatus),
			%% process the rest receipts.
			traverse_delivery_receipts(GatewayId, DlrTime, Receipts);
		Error ->
			Error
	end.

register_delivery_receipt(MsgInfo, DlrTime, MessageState) ->
	{ok, Item} = build_receipt_item(MsgInfo, DlrTime, MessageState),
	ok = k_mailbox:register_incoming_item(Item).

build_receipt_item(#msg_info{
	client_type = k1api,
	customer_id = CustomerId,
	user_id = _UserId,
	in_msg_id = InMsgId,
	src_addr = SrcAddr,
	dst_addr = DstAddr
}, _DlrTime, MsgState) ->
	ItemId = uuid:newid(),
	Item = #k_mb_k1api_receipt{
		id = ItemId,
		customer_id	= CustomerId,
		user_id	= <<"undefined">>,
		source_addr = SrcAddr,
		dest_addr = DstAddr,
		input_message_id = InMsgId,
		message_state = MsgState
	},
	{ok, Item};
build_receipt_item(#msg_info{
	client_type = funnel,
	customer_id = CustomerId,
	user_id = _UserId,
	in_msg_id = InMsgId,
	src_addr = SrcAddr,
	dst_addr = DstAddr
}, DlrTime, MsgState) ->
	ItemId = uuid:newid(),
	Item = #k_mb_funnel_receipt{
		id = ItemId,
		customer_id	= CustomerId,
		user_id = <<"undefined">>,
		source_addr = SrcAddr,
		dest_addr = DstAddr,
		input_message_id = InMsgId,
		submit_date = DlrTime,
		done_date = DlrTime,
		message_state = MsgState
	 },
	{ok, Item}.
