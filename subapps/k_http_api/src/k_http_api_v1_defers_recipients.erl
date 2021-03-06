-module(k_http_api_v1_defers_recipients).

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
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/defers/:req_id/recipients"
    }}.

read(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_defers:get_recipients(ReqId) of
        {ok, Resp} ->
            {ok, Resp};
        {error, no_entry} ->
            {http_code, 404}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
