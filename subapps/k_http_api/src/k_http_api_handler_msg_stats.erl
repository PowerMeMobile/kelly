-module(k_http_api_handler_msg_stats).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = from, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = to, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = type, mandatory = true, repeated = false, type = atom},
        #param{name = slice_length, mandatory = false, repeated = false, type = string}
    ],
    {ok, #specs{
        read = Read,
        route = "/report/messages/:type"
    }}.

read(Params) ->
    ?log_debug("Params: ~p", [Params]),
    Type = ?gv(type, Params),
    From = ?gv(from, Params),
    To = ?gv(to, Params),
    case build_report(From, To, Type, Params) of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Messages stats report failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

build_report(From, To, customers, _Params) ->
    From2 = ac_datetime:datetime_to_timestamp(From),
    To2 = ac_datetime:datetime_to_timestamp(To),
    k_statistic_msg_stats_report:get_report(customers, From2, To2);

build_report(From, To, networks, _Params) ->
    From2 = ac_datetime:datetime_to_timestamp(From),
    To2 = ac_datetime:datetime_to_timestamp(To),
    k_statistic_msg_stats_report:get_report(networks, From2, To2);

build_report(From, To, details, Params) ->
    From2 = ac_datetime:datetime_to_unixepoch(From),
    To2 = ac_datetime:datetime_to_unixepoch(To),
    SliceLengthSecs = convert_slice_length(?gv(slice_length, Params)),
    k_statistic_detailed_msg_stats_report:get_report(From2, To2, SliceLengthSecs).

convert_slice_length(undefined) ->
    60;
convert_slice_length([]) ->
    60;
convert_slice_length("S" ++ Length) ->
    list_to_integer(Length);
convert_slice_length("M" ++ Length) ->
    60 * list_to_integer(Length);
convert_slice_length("H" ++ Length) ->
    60 * 60 * list_to_integer(Length);
convert_slice_length("D" ++ Length) ->
    24 * 60 * 60 * list_to_integer(Length);
convert_slice_length(Length) ->
    list_to_integer(Length).
