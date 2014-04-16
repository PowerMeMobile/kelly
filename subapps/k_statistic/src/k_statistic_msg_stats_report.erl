-module(k_statistic_msg_stats_report).

-export([
    get_report/3
]).

-include_lib("k_storage/include/network.hrl").
-include_lib("k_common/include/logging.hrl").

-type report_type() :: customers | networks.
-type reason() :: any().
-type address() :: binary().
-type prefix() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(
    report_type(),
    erlang:timestamp(),
    erlang:timestamp()
) -> {ok, term()} | {error, reason()}.
get_report(ReportType, From, To) when From < To ->
    {ok, MtRecords} = get_records(mt_messages, From, To),
    {ok, NetworkIdPrefixMap} = get_network_id_prefix_map(),

    RawRecords = build_raw_records(MtRecords, NetworkIdPrefixMap),
    Report = build_msg_stats_report(ReportType, RawRecords),
    Annotate = annotate_msg_stats_report(ReportType, Report),

    AnnotatedReport = {ReportType, Annotate},
    {ok, AnnotatedReport}.

%% ===================================================================
%% Internal
%% ===================================================================

get_records(Collection, From, To) ->
    Selector = {
        'rqt' , {
            '$gte' , From,
            '$lt'  , To
        }
    },
    Projector = {
        'imi'  , 1,
        'ci'   , 1,
        'da.a' , 1
    },
    case shifted_storage:find(Collection, Selector, Projector) of
        {ok, Docs} ->
            {ok, [strip_doc(Doc) || {_Id, Doc} <- Docs]};
        Error ->
            Error
    end.

strip_doc(Doc) ->
    InMsgId = bsondoc:at(imi, Doc),
    CustomerId = bsondoc:at(ci, Doc),
    {a, DstAddr} = bsondoc:at(da, Doc),
    {InMsgId, CustomerId, DstAddr}.

build_raw_records(Records, NetworkIdPrefixMap) ->
    RawRecords = lists:foldl(
        fun({InMsgId, CustomerId, DstAddr}, RawRecords) ->
            case lookup_network_id(DstAddr, NetworkIdPrefixMap) of
                {value, {NetworkId, _}} ->
                    [{CustomerId, NetworkId, InMsgId} | RawRecords];
                false ->
                    %% unusual case. log the error, and leave the state unchanged.
                    ?log_error("Impossible to determine NetworkId from CustomerId: ~p Address: ~p",
                        [CustomerId, DstAddr]),
                    RawRecords
            end
        end,
        [],
        Records
    ),
    RawRecords.

-spec build_msg_stats_report(report_type(), Records::[tuple()]) -> [tuple()].
build_msg_stats_report(customers, Records) ->
    GroupByCustomer = msg_stats_report(1, Records),
    [{K, msg_stats_report(1, List)} || {K, List} <- GroupByCustomer];

build_msg_stats_report(networks, Records) ->
    GroupByNetwork = msg_stats_report(2, Records),
    [{K, msg_stats_report(1, List)} || {K, List} <- GroupByNetwork].

-spec annotate_msg_stats_report(report_type(), Records::[tuple()]) -> [tuple()].
annotate_msg_stats_report(customers, Records) ->
    lists:map(
        fun({CustomerId, Networks}) ->
            [
                {id, CustomerId},
                {networks, [
                    [{id, NetworkId}, {mids, MsgIds} ] || {NetworkId, MsgIds} <- Networks]
                }
            ]
        end,
        Records
    );

annotate_msg_stats_report(networks, Records) ->
    lists:map(
        fun({NetworkId, Customers}) ->
            [
                {id, NetworkId},
                {customers, [
                    [{id, CustomerId}, {mids, MsgIds}] || {CustomerId, MsgIds} <- Customers
                ]}
            ]
        end,
        Records
    ).

-spec msg_stats_report(KeyN::integer(), [tuple()]) -> [tuple()].
msg_stats_report(KeyN, Records) ->
    Pairs = [ac_lists:make_pair(KeyN, R) || R <- Records],
    Dict = lists:foldl(
        fun({K, V}, Dict) ->
            orddict:append(K, V, Dict)
        end,
        orddict:new(),
        Pairs),
    orddict:to_list(Dict).

-spec lookup_network_id(address(), [{network_id(), prefix()}]) ->
    {value, {network_id(), prefix()}} | false.
lookup_network_id(Address, Map) ->
    ac_lists:findwith(
        fun({_, Prefix}) ->
            binary:longest_common_prefix([Prefix, Address]) =:= size(Prefix)
        end,
        Map
    ).

-spec get_network_id_prefix_map() ->
    {ok, [{network_id(), prefix()}]} | {error, reason()}.
get_network_id_prefix_map() ->
    case k_storage_networks:get_networks() of
        {ok, Networks} ->
            {ok, lists:flatmap(
                    fun({NetworkId, Network}) ->
                        Code = Network#network.country_code,
                        Prefixes = Network#network.prefixes,
                        [
                            {NetworkId, <<Code/binary, Prefix/binary>>} ||
                            Prefix <- Prefixes
                        ]
                    end,
                    Networks
                 )
            };
        Error ->
            Error
    end.
