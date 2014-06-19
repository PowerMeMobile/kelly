-module(k_http_api_handler_mt_msg_stats).

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
        #param{name = skip, mandatory = true, repeated = false, type = integer},
        #param{name = limit, mandatory = true, repeated = false, type = integer},
        #param{name = order_by, mandatory = true, repeated = false, type = binary},
        #param{name = order_direction, mandatory = true, repeated = false, type = binary},
        #param{name = customer_id, mandatory = false, repeated = false, type = binary},
        #param{name = recipient, mandatory = false, repeated = false, type = binary},
        #param{name = status, mandatory = false, repeated = false, type = binary}
    ],
    {ok, #specs{
        read = Read,
        route = "/report/mt"
    }}.

read(Params) ->
    {ok, k_statistic_mt_messages:build_report(Params)}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
