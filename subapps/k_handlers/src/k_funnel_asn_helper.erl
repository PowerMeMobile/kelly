-module(k_funnel_asn_helper).

-include_lib("k_common/include/FunnelAsn.hrl").
-include_lib("k_common/include/logging.hrl").

-export([
	render_outgoing_batch/5,
	render_receipts/1,
	render_receipt/1
]).

-spec render_outgoing_batch( any(), any(), any(), binary(), { atom(), any() } ) -> binary().
render_outgoing_batch(BatchId, Source, Dest, MessageBody, DataCoding) ->
	Msg = #'OutgoingMessage'{
		source = Source,
		dest = Dest,
		dataCoding = DataCoding,
		message = MessageBody
	},
	Batch = #'OutgoingBatch'{
		id = BatchId,
		messages = [ Msg ]
	},
	{ok, DeepList} = 'FunnelAsn':encode('OutgoingBatch', Batch),
	list_to_binary( DeepList ).

render_receipts(Data) ->
	render_receipts(Data, []).

render_receipts([], Acc) -> {ok, Acc};
render_receipts([Data | SoFar], Acc) ->
	render_receipts(SoFar, [render_receipt(Data) | Acc ]).

render_receipt(Data) ->
	{CustomerId, InputMsgId, MessageState, SrcAddr, DstAddr, DlrTime} = Data,
	Receipt = #'DeliveryReceipt'{
		messageId = InputMsgId,
		submitDate = unix_to_utc(DlrTime),
		doneDate = unix_to_utc(DlrTime),
		messageState = MessageState,
		source = SrcAddr,
		dest = DstAddr
	},
	BatchId = k_uuid:to_string(k_uuid:newid()),
	Batch = #'ReceiptBatch'{
		id = BatchId,
		receipts = [Receipt]
	},
	{ok, DeepList} = 'FunnelAsn':encode('ReceiptBatch', Batch),
	Rendered = list_to_binary(DeepList),
	% ?log_debug("Rendered receipt batch: ~p", [Rendered]),
	{CustomerId, BatchId, Rendered}.

unix_to_utc(TS) ->
	NM = TS div 1000000,
	NS = TS - (NM * 1000000),
	T = {NM, NS, 0},
	{ {YY, MM, DD}, {H, M, S} } = calendar:now_to_universal_time(T),
	lists:map( fun(C) -> case C of $\  -> $0; _ -> C end end, lists:flatten( io_lib:format(
		"~4B~2B~2B~2B~2B~2B", [YY, MM, DD, H, M, S]
	) )).
