-module(k_statistic).

-export([
	%% store
	store_outgoing_msg_stats/3,
	store_incoming_msg_stats/3,
	store_status_stats/5,

	%% reports
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
%% Store API
%% ===================================================================

-spec store_outgoing_msg_stats(
	InputId::msg_id(),
	MsgInfo ::#msg_info{},
	Time :: unix_epoch()
) -> ok | {error, Reason::any()}.
store_outgoing_msg_stats(InputId, MsgInfo, Time) ->
	k_statistic_stats:store_outgoing_msg_stats(InputId, MsgInfo, Time).

-spec store_status_stats(
	InputId::msg_id(),
	OutputId::msg_id(),
	MsgInfo::#msg_info{},
	MsgStatus::#msg_status{},
	Time::unix_epoch()
) -> ok | {error, Reason::any()}.
store_status_stats(InputId, OutputId, MsgInfo, MsgStatus, Time) ->
	k_statistic_stats:store_status_stats(InputId, OutputId, MsgInfo, MsgStatus, Time).

-spec store_incoming_msg_stats(
	OutputId::msg_id(),
	MsgInfo::#msg_info{},
	Time::unix_epoch()
) -> ok | {error, Reason::any()}.
store_incoming_msg_stats(OutputId, MsgInfo, Time) ->
	k_statistic_stats:store_incoming_msg_stats(OutputId, MsgInfo, Time).

%% ===================================================================
%% Reports API
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
	From::calendar:datetime(),
	To::calendar:datetime()
) -> {ok, Report::term()} | {error, Reason::any()}.
status_stats_report(From, To) when From < To ->
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
