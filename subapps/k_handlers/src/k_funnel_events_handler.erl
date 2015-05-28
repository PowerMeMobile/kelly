-module(k_funnel_events_handler).

-export([process/1]).

-include("amqp_worker_reply.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/mailbox.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec process(Req :: k_amqp_req:req()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(Req) ->
    {ok, ContentType} = k_amqp_req:content_type(Req),
    {ok, Payload} = k_amqp_req:payload(Req),
    process(ContentType, Payload).

%% ===================================================================
%% Internals
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(<<"ConnectionDownEvent">>, Message) ->
    ?log_debug("Got funnel ConnectionDownEvent", []),
    case adto:decode(#funnel_client_offline_event_dto{}, Message) of
        {ok, #funnel_client_offline_event_dto{
            connection_id = ConnectionId,
            customer_id = CustomerId,
            user_id = UserId
        }} ->
            process_connection_down_event(ConnectionId, CustomerId, UserId);
        {error, Error} ->
            ?log_warn("Failed to decode funnel client offline event: ~p with error: ~p", [Message, Error]),
            {error, Error}
    end;

process(<<"ConnectionUpEvent">>, Message) ->
    ?log_debug("Got funnel ConnectionUpEvent", []),
    case adto:decode(#funnel_client_online_event_dto{}, Message) of
        {ok, #funnel_client_online_event_dto{
            connection_id = ConnectionId,
            customer_id = CustomerId,
            user_id = UserId,
            type = ConnType
        }} ->
            process_connection_up_event(ConnectionId, CustomerId, UserId, ConnType);
        {error, Error} ->
            ?log_warn("Failed to decode funnel client online event: ~p with: ~p", [Message, Error]),
            {error, Error}
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

process_connection_down_event(ConnectionId, CustomerId, UserId) ->
    case get_customer_uuid_by_id(CustomerId) of
        {ok, CustomerUuid} ->
            case k_mailbox:unregister_subscription(ConnectionId, CustomerUuid, UserId) of
                ok ->
                    {ok, []};
                {error, Error} ->
                    ?log_error("Could not unregister ~p with: ~p", [{CustomerUuid, ConnectionId}, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?log_error("Could not unregister customer_id: ~p with: ~p", [CustomerId, Error]),
            {error, Error}
    end.

process_connection_up_event(ConnectionId, CustomerId, UserId, ConnType)
        when ConnType == receiver orelse ConnType == transceiver ->
    case get_customer_uuid_by_id(CustomerId) of
        {ok, CustomerUuid} ->
            {ok, QNameFmt} = application:get_env(k_handlers, funnel_node_queue_fmt),
            QName = binary:replace(QNameFmt, <<"%id%">>, ConnectionId),
            ?log_debug("RMQ queue of new funnel connection: ~p", [QName]),
            Subscription = #k_mb_funnel_sub{
                    id = ConnectionId,
                    customer_uuid = CustomerUuid,
                    user_id = UserId,
                    priority = 0,
                    queue_name = QName,
                    created_at = ac_datetime:utc_timestamp()
            },
            case k_mailbox:register_subscription(Subscription) of
                ok ->
                    {ok, []};
                {error, Error} ->
                    ?log_error("Could not register ~p with: ~p", [{CustomerUuid, ConnectionId}, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?log_error("Could not register customer_id: ~p with: ~p", [CustomerId, Error]),
            {error, Error}
    end;
process_connection_up_event(_ConnectionId, _CustomerId, _UserId, _ConnType) ->
    %% got transmitter connection up event. nothing to do.
    {ok, []}.

get_customer_uuid_by_id(CustomerId) ->
    case k_storage_customers:get_customer_by_id(CustomerId) of
        {ok, #customer{customer_uuid = CustomerUuid}} ->
            {ok, CustomerUuid};
        {error, Error} ->
            {error, Error}
    end.
