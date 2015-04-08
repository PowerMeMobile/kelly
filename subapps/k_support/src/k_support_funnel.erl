-module(k_support_funnel).

-export([
    get_connections/0
]).

-include_lib("alley_dto/include/adto.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec get_connections() -> {ok, [{atom(), term()}]} | {error, term()}.
get_connections() ->
    {ok, CtrlQueue} = application:get_env(k_handlers, funnel_control_queue),
    {ok, ReqBin} = adto:encode(#funnel_connections_request_dto{}),
    case k_support_rmq:rpc_call(CtrlQueue, <<"ConnectionsRequest">>, ReqBin) of
        {ok, <<"ConnectionsResponse">>, RespBin} ->
            {ok, #funnel_connections_response_dto{connections = Connections}} =
                adto:decode(#funnel_connections_response_dto{}, RespBin),
            {ok, _ConnectionPropLists} = prepare_conns(Connections);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

prepare_conns(ConnList) when is_list(ConnList) ->
    prepare_conns(ConnList, []).

prepare_conns([], Acc) ->
    {ok, Acc};
prepare_conns([#funnel_connection_dto{
    connection_id = ConnectionId,
    remote_ip = RemoteIp,
    customer_id = CustomerId,
    user_id = UserId,
    connected_at = ConnectedAt,
    type = Type,
    msgs_received = MsgsReceived,
    msgs_sent = MsgsSent,
    errors = Errors
} | Rest], Acc) ->
    ConnectedAtDT = ac_datetime:utc_string_to_datetime(ConnectedAt),
    ConnectedAtISO = ac_datetime:datetime_to_iso8601(ConnectedAtDT),
    ConnPropList = [
        {id, ConnectionId},
        {remote_ip, RemoteIp},
        {customer_id, CustomerId},
        {user_id, UserId},
        {connected_at, ConnectedAtISO},
        {type, Type},
        {msgs_received, MsgsReceived},
        {msgs_sent, MsgsSent},
        {errors, [error_to_proplist(Error) || Error <- Errors]}
    ],
    prepare_conns(Rest, [ConnPropList | Acc]).

error_to_proplist(#error_dto{error_code = ErrorCode, timestamp = Timestamp}) ->
    [
        {error_code, ErrorCode},
        {timestamp, Timestamp}
    ].
