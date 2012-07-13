-module(k_statistic).

-export([
	store_msg_stats/3,
	store_gtw_stats/3,
	store_status_stats/5,

	msg_stats_report/3,
	gtw_stats_report/2,

	status_stats_report/2,
	status_stats_report/3,

	uplink_report/0,
	downlink_report/0
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").
-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% Store API
%% ===================================================================

-spec store_msg_stats(msg_id(), #msg_info{}, unix_epoch()) -> ok | {error, any()}.
store_msg_stats(InputId, MsgInfo = #msg_info{}, Time) ->
	k_statistic_msg_stats:store_msg_stats(InputId, MsgInfo, Time).

-spec store_gtw_stats(gateway_id(), integer(), unix_epoch()) -> ok | {error, any()}.
store_gtw_stats(GatewayId, Number, Time) ->
	k_statistic_gtw_stats:store_gtw_stats(GatewayId, Number, Time).

-spec store_status_stats(msg_id(), msg_id(), #msg_info{}, #msg_status{}, unix_epoch()) -> ok | {error, any()}.
store_status_stats(InputId, OutputId, MsgInfo, MsgStatus, Time) ->
	k_statistic_status_stats:store_status_stats(InputId, OutputId, MsgInfo, MsgStatus, Time).

%% ===================================================================
%% Reports API
%% ===================================================================

-spec msg_stats_report(ReportType::integer(), From::calendar:datetime(), To::calendar:datetime()) -> {ok, term()} | {error, Reason::any()}.
msg_stats_report(ReportType, From, To) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_reports:msg_stats_report(ReportType, FromUnix, ToUnix).

-spec gtw_stats_report(From::calendar:datetime(), To::calendar:datetime()) -> {ok, term()} | {error, Reason::any()}.
gtw_stats_report(From, To) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_reports:gtw_stats_report(FromUnix, ToUnix).

-spec status_stats_report(From::calendar:datetime(), To::calendar:datetime()) -> {ok, term()} | {error, Reason::any()}.
status_stats_report(From, To) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_reports:status_stats_report(FromUnix, ToUnix, undefined).

-spec status_stats_report(From::calendar:datetime(), To::calendar:datetime(), Status::atom()) -> {ok, term()} | {error, Reason::any()}.
status_stats_report(From, To, Status) when From < To ->
	FromUnix = k_datetime:datetime_to_unix_epoch(From),
	ToUnix = k_datetime:datetime_to_unix_epoch(To),
	k_statistic_reports:status_stats_report(FromUnix, ToUnix, Status).

uplink_report() ->
	k_statistic_uplink_stats:get_stats().

downlink_report() ->
	k_statistic_downlink_stats:get_stats().

