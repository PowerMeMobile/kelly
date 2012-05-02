-module(k_incoming_sms_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/JustAsn.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(undefined, Message) -> % in future - CT will be <<"IncomingSm">>
	case 'JustAsn':decode('IncomingSm', Message) of
		{ok, #'IncomingSm'{
			source = SourceFullAddr,
			dest = DestFullAddr,
			message = MessageBody,
			dataCoding = DataCoding
		}} ->
			#'FullAddr'{
				addr = Addr,
				ton = TON,
				npi = NPI
			} = DestFullAddr,
			case k_addr2cust:resolve({Addr, TON, NPI}) of
				{ok, CID} ->
					?log_debug("Got incoming message for [cust:~p] (addr:~p, ton:~p, npi:~p)", [CID, Addr, TON, NPI] ),
					DC = case DataCoding of
						0 -> {text, gsm0338};
						8 -> {text, ucs2};
						_ -> {other, DataCoding}
					end,
					ItemID = k_uuid:to_string(k_uuid:newid()),
					?log_debug("Incomming message registered [item:~p]", [ItemID]),
					k_mailbox:register_incoming_item(
						ItemID,
						CID, <<"OutgoingBatch">>,
						k_funnel_asn_helper:render_outgoing_batch(
							ItemID, SourceFullAddr,
							DestFullAddr, MessageBody, DC
						));
				_ ->
					?log_info("Could not resolve incoming message to (addr:~p, ton:~p, npi:~p)", [Addr, TON, NPI])
			end,
			{ok, []};
		{error, Err} ->
			{error, Err}
	end;

process(CT, Message) ->
	?log_warn("Got unexpected message of type ~p: ~p", [CT, Message]),
	{ok, []}.
