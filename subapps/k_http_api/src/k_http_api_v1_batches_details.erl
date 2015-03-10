-module(k_http_api_v1_batches_details).

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
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        read = Read,
        route = "/v1/batches/:req_id"
    }}.

read(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_statistic_mt_batches:get_one(ReqId) of
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
