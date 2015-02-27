-module(k_http_api_handler_mt_msg_aggr_stats).

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
        %% customer_id is deprecated, force clients to use customer_uuid
        #param{name = customer_id, mandatory = false, repeated = false, type = uuid},
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = group_by , mandatory = true, repeated = false, type =
            {custom, fun convert_group_by/1}}
    ],
    {ok, #specs{
        read = Read,
        route = "/report/mt_aggr"
    }}.

read(Params) ->
    CustomerUuid = get_customer_uuid(Params),
    Params2 = [{customer_uuid, CustomerUuid} | Params],
    {ok, k_statistic_mt_aggr_reports:summary(Params2)}.

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

get_customer_uuid(Params) ->
    case ?gv(customer_uuid, Params) of
        undefined ->
            ?gv(customer_id, Params);
        CustomerUuid ->
            CustomerUuid
    end.
