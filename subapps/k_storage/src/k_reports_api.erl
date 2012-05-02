-module(k_reports_api).

-export([
	store_msg_stats/3,
	store_gtw_stats/3,

	msg_stats_report/3,
	gtw_stats_report/2
]).

-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/storages.hrl").

%% Public APIs

-spec store_msg_stats(msg_id(), #msg_info{}, integer()) -> ok | {error, any()}.
store_msg_stats(InputId, MsgInfo = #msg_info{}, Time) ->
	k_storage_msg_stats:store_msg_stats(InputId, MsgInfo, Time).

-spec store_gtw_stats(gateway_id(), integer(), integer()) -> ok | {error, any()}.
store_gtw_stats(GatewayId, Number, Time) ->
	k_storage_gtw_stats:store_gtw_stats(GatewayId, Number, Time).

-spec msg_stats_report(ReportType::integer(), From::calendar:datetime(), To::calendar:datetime()) -> {ok, term()} | {error, Reason::any()}.
msg_stats_report(ReportType, From, To) when From < To ->
	FromUnix = k_storage_util:datetime_to_unix_epoch(From),
	ToUnix = k_storage_util:datetime_to_unix_epoch(To),
	k_storage_reports:msg_stats_report(ReportType, FromUnix, ToUnix).

-spec gtw_stats_report(From::calendar:datetime(), To::calendar:datetime()) -> {ok, term()} | {error, Reason::any()}.
gtw_stats_report(From, To) when From < To ->
	FromUnix = k_storage_util:datetime_to_unix_epoch(From),
	ToUnix = k_storage_util:datetime_to_unix_epoch(To),
	k_storage_reports:gtw_stats_report(FromUnix, ToUnix).
