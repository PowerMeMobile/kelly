-module(k_http_api_v1_reports_mt_aggr_by_period).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
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
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = group_by , mandatory = true, repeated = false, type =
            {custom, fun convert_group_by/1}}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/reports/mt_aggr_by_period"
    }}.

read(Params) ->
    {ok, k_statistic_mt_aggr_reports:by_period(Params)}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

convert_group_by(<<"m">>) ->
    monthly;
convert_group_by(<<"d">>) ->
    daily;
convert_group_by(<<"h">>) ->
    hourly.
