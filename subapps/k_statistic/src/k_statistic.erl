-module(k_statistic).

-export([
	status_stats_report/2,
	status_stats_report/3,

	msg_stats_report/3,
	detailed_msg_stats_report/3,

	uplink_report/0,
	downlink_report/0
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec status_stats_report(
	FromDate::calendar:datetime(),
	ToDate::calendar:datetime()
) -> {ok, Report::term()} | {error, Reason::any()}.
status_stats_report(FromDate, ToDate) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_status_stats_report:get_report(From, To).

-spec status_stats_report(
	FromDate::calendar:datetime(),
	ToDate::calendar:datetime(),
	Status::atom()
) -> {ok, Report::term()} | {error, Reason::any()}.
status_stats_report(FromDate, ToDate, Status) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_status_stats_report:get_report(From, To, Status).

-spec msg_stats_report(
	ReportType::integer(),
	FromDate::calendar:datetime(),
	ToDate::calendar:datetime()
) -> {ok, Report::term()} | {error, Reason::any()}.
msg_stats_report(ReportType, FromDate, ToDate) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_msg_stats_report:get_report(ReportType, From, To).

-spec detailed_msg_stats_report(
	FromDate::calendar:datetime(),
	ToDate::calendar:datetime(),
	SliceLength::pos_integer()
) -> {ok, Report::term()} | {error, Reason::term()}.
detailed_msg_stats_report(FromDate, ToDate, SliceLength) when FromDate < ToDate ->
	FromUnix = k_datetime:datetime_to_unixepoch(FromDate),
	ToUnix = k_datetime:datetime_to_unixepoch(ToDate),
	k_statistic_detailed_msg_stats_report:get_report(FromUnix, ToUnix, SliceLength).

-spec uplink_report() -> {ok, Report::term()} | {error, Reason::term()}.
uplink_report() ->
	k_statistic_uplink_stats_report:get_report().

-spec downlink_report() -> {ok, Report::term()} | {error, Reason::term()}.
downlink_report() ->
	k_statistic_downlink_stats_report:get_report().
