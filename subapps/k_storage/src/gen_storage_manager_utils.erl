-module(gen_storage_manager_utils).

-export([
    get_prev_shift_time/2,
    get_curr_shift_time/2,
    get_next_shift_time/2,
%%  get_response_end_time/2,
%%  get_delivery_end_time/2,
    add_frame/2,
    to_seconds/1
]).

-type frame() :: {seconds | minutes | hours | days | months, pos_integer()}.

%% ===================================================================
%% API
%% ===================================================================

-spec get_prev_shift_time(calendar:datetime(), frame()) -> calendar:datetime().
get_prev_shift_time(CurrTime, {months, 1}) ->
    {{Year, Month, _}, _} = CurrTime,
    {PrevYear, PrevMonth, _} = edate:shift({Year, Month, 1}, -1, month),
    {{PrevYear, PrevMonth, 1}, {0, 0, 0}}.

-spec get_curr_shift_time(calendar:datetime(), frame()) -> calendar:datetime().
get_curr_shift_time(CurrTime, {months, 1}) ->
    {{Year, Month, _}, _} = CurrTime,
    {{Year, Month, 1}, {0, 0, 0}}.

-spec get_next_shift_time(calendar:datetime(), frame()) -> calendar:datetime().
get_next_shift_time(CurrTime, {months, N}) ->
    {{Year, Month, _}, _} = CurrTime,
    {NextYear, NextMonth, NextDay} = edate:shift({Year, Month, 1}, N, month),
    {{NextYear, NextMonth, NextDay}, {0, 0, 0}}.

%% -spec get_response_end_time(calendar:datetime(), frame()) -> calendar:datetime().
%% get_response_end_time(ShiftTime, Frame) ->
%%  add_frame(ShiftTime, Frame).

%% -spec get_delivery_end_time(calendar:datetime(), frame()) -> calendar:datetime().
%% get_delivery_end_time(ShiftTime, Frame) ->
%%  add_frame(ShiftTime, Frame).

-spec add_frame(calendar:datetime(), frame()) -> calendar:datetime().
add_frame(Datetime, Frame) ->
    DatetimeSecs = calendar:datetime_to_gregorian_seconds(Datetime),
    ResultSecs = DatetimeSecs + to_seconds(Frame),
    calendar:gregorian_seconds_to_datetime(ResultSecs).

-spec to_seconds(frame()) -> non_neg_integer().
to_seconds({seconds, N}) ->
    N;
to_seconds({minutes, N}) ->
    N*60;
to_seconds({hours, N}) ->
    N*60*60;
to_seconds({days, N}) ->
    N*24*60*60.
