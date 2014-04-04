-module(k_statistic).

-export([
    get_mt_msg_status_report/4,
    get_aggregated_statuses_report/2,
    get_msgs_by_status_report/3,

    get_msg_stats_report/3,
    get_detailed_msg_stats_report/3,

    uplink_report/0,
    downlink_report/0
]).

-include_lib("k_storage/include/msg_info.hrl").
-include_lib("k_storage/include/storages.hrl").

-type report() :: term().
-type reason() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec get_mt_msg_status_report(
    customer_id(), user_id(), client_type(), in_msg_id()
) -> {ok, report()} | {error, reason()}.
get_mt_msg_status_report(CustomerId, UserId, ClientType, InMsgId) ->
    k_statistic_status_reports:get_mt_msg_status_report(CustomerId, UserId, ClientType, InMsgId).

-spec get_aggregated_statuses_report(calendar:datetime(), calendar:datetime()) ->
    {ok, report()} | {error, reason()}.
get_aggregated_statuses_report(FromDate, ToDate) when FromDate < ToDate ->
    From = ac_datetime:datetime_to_timestamp(FromDate),
    To = ac_datetime:datetime_to_timestamp(ToDate),
    k_statistic_status_reports:get_aggregated_statuses_report(From, To).

-spec get_msgs_by_status_report(calendar:datetime(), calendar:datetime(), status()) ->
    {ok, report()} | {error, reason()}.
get_msgs_by_status_report(FromDate, ToDate, Status) when FromDate < ToDate ->
    From = ac_datetime:datetime_to_timestamp(FromDate),
    To = ac_datetime:datetime_to_timestamp(ToDate),
    k_statistic_status_reports:get_msgs_by_status_report(From, To, Status).

-spec get_msg_stats_report(integer(), calendar:datetime(), calendar:datetime()) ->
    {ok, report()} | {error, reason()}.
get_msg_stats_report(ReportType, FromDate, ToDate) when FromDate < ToDate ->
    From = ac_datetime:datetime_to_timestamp(FromDate),
    To = ac_datetime:datetime_to_timestamp(ToDate),
    k_statistic_msg_stats_report:get_report(ReportType, From, To).

-spec get_detailed_msg_stats_report(calendar:datetime(), calendar:datetime(), pos_integer()) ->
    {ok, report()} | {error, reason()}.
get_detailed_msg_stats_report(FromDate, ToDate, SliceLength) when FromDate < ToDate ->
    FromUnix = ac_datetime:datetime_to_unixepoch(FromDate),
    ToUnix = ac_datetime:datetime_to_unixepoch(ToDate),
    k_statistic_detailed_msg_stats_report:get_report(FromUnix, ToUnix, SliceLength).

-spec uplink_report() -> {ok, report()} | {error, reason()}.
uplink_report() ->
    k_statistic_uplink_stats_report:get_report().

-spec downlink_report() -> {ok, report()} | {error, reason()}.
downlink_report() ->
    k_statistic_downlink_stats_report:get_report().
