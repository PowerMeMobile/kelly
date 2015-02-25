-module(k_statistic_utils).

-export([
    get_timestamp_ranges/3
]).

-include("application.hrl").

-type unixepoch() :: pos_integer().

%% ===================================================================
%% API
%% ===================================================================

-spec get_timestamp_ranges(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) -> [{unixepoch(), unixepoch()}].
get_timestamp_ranges(From, To, Step) when From < To ->
    Timestamps = get_timestamp_list(From, To, Step),
    ac_lists:make_ranges(Timestamps).

%% ===================================================================
%% Internal
%% ===================================================================

-spec stats_report_frequency() -> integer().
stats_report_frequency() ->
    60.

-spec get_timestamp_list(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) ->
    [unixepoch()].
get_timestamp_list(From, To, Step) when From < To ->
    {FromFloor, ToCeiling} = align_time_range(From, To),
    List = lists:seq(FromFloor, ToCeiling, Step),
    case lists:last(List) < To of
        true -> List ++ [To];
        false -> List
    end.

-spec align_time_range(From::unixepoch(), To::unixepoch()) ->
    {FromFloor::unixepoch(), ToCeiling::unixepoch()}.
align_time_range(From, To) ->
    Step = stats_report_frequency(),
    align_time_range(From, To, Step).

-spec align_time_range(
    From::unixepoch(), To::unixepoch(), Step::pos_integer()
) ->
    {FromFloor::unixepoch(), ToCeiling::unixepoch()}.
align_time_range(From, To, Step) ->
    FromFloor = From - From rem Step,
    ToCeiling = case To rem Step of
                    0 -> To;
                    Rem -> To - Rem + Step
                end,
    {FromFloor, ToCeiling}.
