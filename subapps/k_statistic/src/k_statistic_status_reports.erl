-module(k_statistic_status_reports).

-export([
    build_report/1
]).

-include_lib("k_storage/include/msg_info.hrl").
-include_lib("alley_common/include/utils.hrl").

-type report() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec build_report(proplists:proplist()) -> {ok, report()} | {error, reason()}.
build_report(Params) ->
    From = ac_datetime:datetime_to_timestamp(?gv(from, Params)),
    To = ac_datetime:datetime_to_timestamp(?gv(to, Params)),
    CustomerUuid = ?gv(customer_uuid, Params),
    DealerUuid = ?gv(dealer_uuid, Params),
    Status = ?gv(status, Params),
    if
        Status =:= undefined ->
            get_aggregated_statuses_report(From, To, CustomerUuid, DealerUuid);
        true ->
            get_msgs_by_status_report(From, To, CustomerUuid, DealerUuid, Status)
    end.


-spec get_aggregated_statuses_report(timestamp(), timestamp(), undefined | customer_id(), dealer_id()) ->
    {ok, report()} | {error, reason()}.
get_aggregated_statuses_report(From, To, CustomerUuid, DealerUuid) ->
    Query =
    if
        CustomerUuid =/= undefined ->
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuid};
        DealerUuid =/= undefined ->
            {ok, DealerCustomersUuidList} =
                k_storage_customers:get_customers_uuid_by_dealer_uuid(DealerUuid),
            CustomerUuidSelector = {'$in', DealerCustomersUuidList},
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuidSelector};
        true ->
            {'rqt', {'$gte', From, '$lt', To}}
    end,

    MtCommand = {
        'aggregate', <<"mt_messages">>,
        'pipeline' , [
            {'$match', Query},
            {'$group', {'_id', <<"$s">>, value, {'$sum', 1}}}
        ]
    },
    MoCommand = {
        'aggregate', <<"mo_messages">>,
        'pipeline' , [
            {'$match', Query},
            {'$group', {'_id', <<"received">>, value, {'$sum', 1}}}
        ]
    },
    {ok, MtDocs} = shifted_storage:command(MtCommand),
    {ok, MoDocs} = shifted_storage:command(MoCommand),
    Docs = MtDocs ++ MoDocs,
    Results = merge([
        {binary_to_existing_atom(Status, latin1), round(Hits)}
        || {'_id', Status, value, Hits} <- Docs
     ]),
    {ok, Results}.

-spec get_msgs_by_status_report(timestamp(), timestamp(), undefined | customer_id(), dealer_id(), status()) ->
    {ok, report()} | {error, reason()}.
get_msgs_by_status_report(From, To, CustomerUuid, DealerUuid, received) ->
    Selector =
    if
        CustomerUuid =/= undefined ->
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuid};
        DealerUuid =/= undefined ->
            {ok, DealerCustomersUuidList} =
                k_storage_customers:get_customers_uuid_by_dealer_uuid(DealerUuid),
            CustomerUuidSelector = {'$in', DealerCustomersUuidList},
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuidSelector};
        true ->
            {'rqt', {'$gte', From, '$lt', To}}
    end,

    get_raw_report(mo_messages, Selector);

get_msgs_by_status_report(From, To, CustomerUuid, DealerUuid, Status) when
    Status == pending;
    Status == submitted; Status == failed; Status == blocked;
    Status == enroute; Status == delivered; Status == expired;
    Status == deleted; Status == undeliverable; Status == accepted;
    Status == unknown; Status == rejected; Status == unrecognized
->
    StatusBin = bsondoc:atom_to_binary(Status),

    Selector =
    if
        CustomerUuid =/= undefined ->
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuid, 's', StatusBin};
        DealerUuid =/= undefined ->
            {ok, DealerCustomersUuidList} =
                k_storage_customers:get_customers_uuid_by_dealer_uuid(DealerUuid),
            CustomerUuidSelector = {'$in', DealerCustomersUuidList},
            {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerUuidSelector, 's', StatusBin};
        true ->
            {'rqt', {'$gte', From, '$lt', To}, 's', StatusBin}
    end,
    get_raw_report(mt_messages, Selector).

%% ===================================================================
%% Internal
%% ===================================================================

merge(Pairs) ->
    Dict = dict:from_list([
        {received, 0},
        {pending, 0},
        {submitted, 0},
        {failed, 0},
        {blocked, 0},
        {enroute, 0},
        {delivered, 0},
        {expired, 0},
        {deleted, 0},
        {undeliverable, 0},
        {accepted, 0},
        {unknown, 0},
        {rejected, 0},
        {unrecognized, 0}
    ]),
    dict:to_list(merge(Pairs, Dict)).

merge([], Dict) ->
    Dict;
merge([{Key, Value}|Pairs], Dict) ->
    NewDict = dict:update_counter(Key, Value, Dict),
    merge(Pairs, NewDict).

get_raw_report(mt_messages, Selector) ->
    case shifted_storage:find(mt_messages, Selector) of
        {ok, Docs} ->
            Msgs = [k_storage_utils:doc_to_mt_msg_info(Doc) || {_Id, Doc} <- Docs],
            CustUuids = [M#msg_info.customer_uuid || M <- Msgs],
            CustDict = k_storage_utils:get_uuid_to_customer_dict(CustUuids),
            GtwIds = [M#msg_info.gateway_id || M <- Msgs],
            GtwDict = k_storage_utils:get_id_to_gateway_dict(GtwIds),
            MtMsgs = [k_statistic_utils:build_mt_msg_resp(M, CustDict, GtwDict) || M <- Msgs],
            {ok, MtMsgs};
        Error ->
            Error
    end;
get_raw_report(mo_messages, Selector) ->
    case shifted_storage:find(mo_messages, Selector) of
        {ok, Docs} ->
            Msgs = [k_storage_utils:doc_to_mo_msg_info(Doc) || {_Id, Doc} <- Docs],
            CustUuids = [M#msg_info.customer_uuid || M <- Msgs],
            CustDict = k_storage_utils:get_uuid_to_customer_dict(CustUuids),
            GtwIds = [M#msg_info.gateway_id || M <- Msgs],
            GtwDict = k_storage_utils:get_id_to_gateway_dict(GtwIds),
            MoMsgs = [k_statistic_utils:build_mo_msg_resp(M, CustDict, GtwDict) || M <- Msgs],
            {ok, MoMsgs};
        Error ->
            Error
    end.
