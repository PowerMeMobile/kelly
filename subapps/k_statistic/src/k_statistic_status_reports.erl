-module(k_statistic_status_reports).

-export([
    get_aggregated_statuses_report/3,
    get_msgs_by_status_report/4
]).

-include_lib("k_storage/include/msg_info.hrl").

-type report() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_aggregated_statuses_report(timestamp(), timestamp(), undefined | customer_id()) ->
    {ok, report()} | {error, reason()}.
get_aggregated_statuses_report(From, To, CustomerId) ->
    Query =
        case CustomerId of
            undefined ->
                {'rqt', {'$gte', From, '$lt', To}};
            CustomerId ->
                {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerId}
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

-spec get_msgs_by_status_report(timestamp(), timestamp(), undefined | customer_id(), status()) ->
    {ok, report()} | {error, reason()}.
get_msgs_by_status_report(From, To, CustomerId, received) ->
    Selector =
        case CustomerId of
            undefined ->
                {'rqt', {'$gte', From, '$lt', To}};
            CustomerId ->
                {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerId}
        end,
    get_raw_report(mo_messages, Selector);

get_msgs_by_status_report(From, To, CustomerId, Status) when
    Status == pending;
    Status == submitted; Status == failed; Status == blocked;
    Status == enroute; Status == delivered; Status == expired;
    Status == deleted; Status == undeliverable; Status == accepted;
    Status == unknown; Status == rejected; Status == unrecognized
->
    StatusBin = bsondoc:atom_to_binary(Status),
    Selector =
        case CustomerId of
            undefined ->
                {'rqt', {'$gte', From, '$lt', To}, 's', StatusBin};
            CustomerId ->
                {'rqt', {'$gte', From, '$lt', To}, 'ci', CustomerId, 's', StatusBin}
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

get_raw_report(Collection, Selector) ->
    case shifted_storage:find(Collection, Selector) of
        {ok, Docs} ->
            {ok, [doc_to_msg(Collection, Doc) || {_Id, Doc} <- Docs]};
        Error ->
            Error
    end.

doc_to_msg(mt_messages, Doc) ->
    k_statistic_utils:doc_to_mt_msg(Doc);
doc_to_msg(mo_messages, Doc) ->
    k_statistic_utils:doc_to_mo_msg(Doc).
