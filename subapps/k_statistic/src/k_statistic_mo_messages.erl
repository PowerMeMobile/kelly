-module(k_statistic_mo_messages).

-export([
    build_msgs_report/1,
    build_msg_report/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec build_msgs_report([{atom(), term()}]) -> [[{atom(), term()}]].
build_msgs_report(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    Skip = ?gv(skip, Params),
    Limit = ?gv(limit, Params),
    OrderBy = decode_order_by(?gv(order_by, Params)),
    OrderDirection = decode_order_direction(?gv(order_direction, Params)),
    DealerUuid = ?gv(dealer_uuid, Params),
    CustomerUuid = ?gv(customer_uuid, Params),

    CustomerDealerSelector =
    if
        CustomerUuid =/= undefined ->
            [{'ci', CustomerUuid}];

        DealerUuid =/= undefined ->
            {ok, DealerCustomersUuidList} =
                k_storage_customers:get_customers_uuid_by_dealer_uuid(DealerUuid),
            [{'ci', {'$in', DealerCustomersUuidList}}];

        true -> []
    end,

    RecipientSel =
        case ?gv(recipient, Params) of
            undefined -> [];
            Recipient -> [{'da.a', Recipient}]
        end,
    Selector =
        {'$query',
            bson:document(
                [{'rqt', {'$gte', From, '$lt', To}}] ++
                CustomerDealerSelector ++
                RecipientSel
            ),
         '$orderby', {OrderBy, OrderDirection}
        },
    {ok, Docs} = shifted_storage:find(mo_messages, Selector, {}, Skip, Limit),
    Msgs = [k_storage_utils:doc_to_mo_msg_info(Doc) || {_, Doc} <- Docs],
    CustUuids = [M#msg_info.customer_uuid || M <- Msgs],
    CustDict = k_storage_utils:get_uuid_to_customer_dict(CustUuids),
    GtwIds = [M#msg_info.gateway_id || M <- Msgs],
    GtwDict = k_storage_utils:get_id_to_gateway_dict(GtwIds),
    [k_statistic_utils:build_mo_msg_resp(M, CustDict, GtwDict) || M <- Msgs].

-spec build_msg_report(msg_id()) -> [[{atom(), term()}]].
build_msg_report(MsgId) ->
    Selector = {
        '_id', MsgId
    },
    {ok, Doc} = shifted_storage:find_one(mo_messages, Selector),
    Msgs = [k_storage_utils:doc_to_mo_msg_info(D) || D <- [Doc]],
    CustUuids = [M#msg_info.customer_uuid || M <- Msgs],
    CustDict = k_storage_utils:get_uuid_to_customer_dict(CustUuids),
    GtwIds = [M#msg_info.gateway_id || M <- Msgs],
    GtwDict = k_storage_utils:get_id_to_gateway_dict(GtwIds),
    [k_statistic_utils:build_mo_msg_resp(M, CustDict, GtwDict) || M <- Msgs].

%% ===================================================================
%% Internals
%% ===================================================================

decode_order_by(<<"req_time">>) ->
    rqt;
decode_order_by(<<"status">>) ->
    s;
decode_order_by(<<"customer_name">>) ->
    rqt;
decode_order_by(<<"user_id">>) ->
    rqt;
decode_order_by(<<"client_type">>) ->
    ct;
decode_order_by(<<"dst_addr.addr">>) ->
    'da.a';
decode_order_by(<<"src_addr.addr">>) ->
    'sa.a'.

decode_order_direction(<<"asc">>) ->
    1;
decode_order_direction(<<"desc">>) ->
    -1.
