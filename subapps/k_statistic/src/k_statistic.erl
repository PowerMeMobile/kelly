-module(k_statistic).

-export([
	msg_status_report/3,

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

-type customer_id() :: binary().
-type client_type() :: binary().
-type in_msg_id() :: binary().
-type report() :: term().
-type reason() :: term().
-type status() :: atom().

%% ===================================================================
%% API
%% ===================================================================

-spec msg_status_report(customer_id(), client_type(), in_msg_id()) -> {ok, report()} | {error, reason()}.
msg_status_report(CustomerId, ClientType, InMsgId) ->
	case k_storage:get_mt_msg_info(CustomerId, ClientType, InMsgId) of
		{ok, MsgInfo} ->
			{ok, {
				message, [
					{customer_id, CustomerId},
					{message_id, InMsgId},
					{client_type, ClientType},
					{status, ?MSG_STATUS(MsgInfo)}
				]
			}};
		Error ->
			Error
	end.

-spec status_stats_report(calendar:datetime(), calendar:datetime()) ->
	{ok, report()} | {error, reason()}.
status_stats_report(FromDate, ToDate) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_status_stats_report:get_report(From, To).

-spec status_stats_report(calendar:datetime(), calendar:datetime(), status()) ->
	{ok, report()} | {error, reason()}.
status_stats_report(FromDate, ToDate, Status) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_status_stats_report:get_report(From, To, Status).

-spec msg_stats_report(integer(), calendar:datetime(), calendar:datetime()) ->
	{ok, report()} | {error, reason()}.
msg_stats_report(ReportType, FromDate, ToDate) when FromDate < ToDate ->
	From = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(FromDate)),
	To = k_datetime:unixepoch_to_timestamp(k_datetime:datetime_to_unixepoch(ToDate)),
	k_statistic_msg_stats_report:get_report(ReportType, From, To).

-spec detailed_msg_stats_report(calendar:datetime(), calendar:datetime(), pos_integer()) ->
	{ok, report()} | {error, reason()}.
detailed_msg_stats_report(FromDate, ToDate, SliceLength) when FromDate < ToDate ->
	FromUnix = k_datetime:datetime_to_unixepoch(FromDate),
	ToUnix = k_datetime:datetime_to_unixepoch(ToDate),
	k_statistic_detailed_msg_stats_report:get_report(FromUnix, ToUnix, SliceLength).

-spec uplink_report() -> {ok, report()} | {error, reason()}.
uplink_report() ->
	k_statistic_uplink_stats_report:get_report().

-spec downlink_report() -> {ok, report()} | {error, reason()}.
downlink_report() ->
	k_statistic_downlink_stats_report:get_report().
