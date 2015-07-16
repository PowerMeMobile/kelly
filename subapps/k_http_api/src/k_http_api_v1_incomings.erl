-module(k_http_api_v1_incomings).

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
        #param{name = user_id, mandatory = false, repeated = false, type = binary},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun decode_state/1}},
        #param{name = skip, mandatory = true, repeated = false, type = integer},
        #param{name = limit, mandatory = true, repeated = false, type = integer}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/incomings"
    }}.

read(Params) ->
    {ok, Resp} = k_statistic_incomings:get_all(Params),
    {ok, Resp}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

decode_state(State) ->
    case bstr:lower(State) of
        <<"all">>  -> all;
        <<"new">>  -> new;
        <<"read">> -> read
    end.
