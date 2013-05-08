-module(k_funnel_events_handler).

-export([process/2]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/storages.hrl").
-include_lib("k_mailbox/include/application.hrl").

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ConnectionDownEvent">>, Message) ->
	?log_debug("Got funnel ConnectionDownEvent", []),
	case adto:decode(#funnel_client_offline_event_dto{}, Message) of
		{ok, #funnel_client_offline_event_dto{
			connection_id = ConnId,
			customer_id = SystemId,
			user_id = UserId }} ->
			process_connection_down_event(ConnId, SystemId, UserId);
		{error, Error} ->
			?log_warn("Failed to decode funnel client offline event: ~p with error: ~p", [Message, Error]),
			{ok, []}
	end;

process(<<"ConnectionUpEvent">>, Message) ->
	?log_debug("Got funnel ConnectionUpEvent", []),
	case adto:decode(#funnel_client_online_event_dto{}, Message) of
		{ok, #funnel_client_online_event_dto{
			connection_id = ConnId,
			customer_id = SystemId,
			type = ConnType,
			user_id = UserId }} ->
			process_connection_up_event(ConnId, SystemId, ConnType, UserId);
		{error, Error} ->
			?log_warn("Failed to decode funnel client online event: ~p with: ~p", [Message, Error]),
			{ok, []}
	end;

process(<<"ServerUpEvent">>, _Message) ->
	?log_info("Got funnel ServerUpEvent", []),
	{ok, []};

process(<<"ServerDownEvent">>, _Message) ->
	?log_info("Got funnel ServerDownEvent", []),
	k_mailbox:process_funnel_down_event(),
	{ok, []};

process(Type, _Message) ->
	?log_warn("Got unexpected funnel event message type: ~p", [Type]),
	{ok, []}.


%%% Internal

process_connection_down_event(ConnId, SystemId, UserId) ->
	case resolve_cust_id(SystemId) of
		{ok, CustId} ->
			perform_unregister_connection(CustId, ConnId, UserId);
		{error, Reason} ->
			?log_error("Could not unregister system-id: ~p with: ~p", [SystemId, Reason]),
			{ok, []}
	end.

perform_unregister_connection(CustId, ConnId, UserId) ->
	case k_mailbox:unregister_subscription(ConnId, CustId, UserId) of
		ok ->
			{ok, []};
		{error, Reason} ->
			?log_error("Could not unregister ~p with: ~p", [{CustId, ConnId}, Reason]),
			{ok, []}
	end.

process_connection_up_event(ConnId, SystemId, ConnType, UserId)
	when ConnType == receiver orelse ConnType == transceiver
->
	case resolve_cust_id(SystemId) of
		{ok, CustId} ->
			QName = << <<"pmm.funnel.nodes.">>/binary, ConnId/binary >>,
			?log_debug("RMQ queue of new funnel connection: ~p", [QName]),
			Subscription = #k_mb_funnel_sub{
					id = ConnId,
					customer_id = CustId,
					user_id = UserId,
					priority = 0,
					queue_name = QName,
					created_at = k_datetime:utc_timestamp()
			},
			k_mailbox:register_subscription(Subscription),
			{ok, []};
		{error, Reason} ->
			?log_error("Could not register system-id: ~p with: ~p", [SystemId, Reason]),
			{ok, []}
	end;
process_connection_up_event(_ConnId, _SystemId, _ConnType, _UserID) ->
	%% got transmitter connection up event. nothing to do.
	{ok, []}.

resolve_cust_id(SystemId) ->
	case k_aaa:get_customer_by_system_id(SystemId) of
		{ok, #customer{uuid = CustId}} ->
			{ok, CustId};
		Error ->
			Error
	end.
