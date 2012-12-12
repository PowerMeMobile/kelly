-module(k_statistic).

-export([
	msg_stats_report/3,

	status_stats_report/2,
	status_stats_report/3,

	detailed_msg_stats_report/3,

	uplink_report/0,
	downlink_report/0
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec msg_stats_report(
	ReportType::integer(),
	From::calendar:datetime(),
	To::calendar:datetime()
) -> {ok, Report::term()} | {error, Reason::any()}.
msg_stats_report(ReportType, From, To) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_msg_stats_report:get_report(ReportType, FromUnix, ToUnix).

-spec status_stats_report(
	FromDate::calendar:datetime(),
	ToDate::calendar:datetime()
) -> {ok, Report::term()} | {error, Reason::any()}.
status_stats_report(FromDate, ToDate) when FromDate < ToDate ->
	From = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(FromDate)),
	To = k_datetime:unix_epoch_to_timestamp(k_datetime:datetime_to_unix_epoch(ToDate)),
	k_statistic_status_stats_report:get_report(From, To).

-spec status_stats_report(
	From::calendar:datetime(),
	To::calendar:datetime(),
	Status::atom()
) -> {ok, term()} | {error, Reason::any()}.
status_stats_report(From, To, Status) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_status_stats_report:get_report(FromUnix, ToUnix, Status).

-spec uplink_report() -> {ok, Report::term()} | {error, Reason::term()}.
uplink_report() ->
	k_statistic_uplink_stats_report:get_report().

-spec downlink_report() -> {ok, Report::term()} | {error, Reason::term()}.
downlink_report() ->
	k_statistic_downlink_stats_report:get_report().

-spec detailed_msg_stats_report(
	From::calendar:datetime(),
	To::calendar:datetime(),
	SliceLength::pos_integer()
) -> {ok, Report::term()} | {error, Reason::term()}.
detailed_msg_stats_report(From, To, SliceLength) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_detailed_msg_stats_report:get_report(FromUnix, ToUnix, SliceLength).
