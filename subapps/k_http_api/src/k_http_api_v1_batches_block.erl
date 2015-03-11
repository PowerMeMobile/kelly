-module(k_http_api_v1_batches_block).

-behaviour(gen_http_api).

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
%% Callback Functions
%% ===================================================================

init() ->
    Create = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    Delete = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        create = Create,
        read = undefined,
        update = undefined,
        delete = Delete,
        route = "/v1/batches/:req_id/block"
    }}.

read(_Params) ->
    ok.

create(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_statistic_mt_batches:get_details(ReqId) of
        {ok, _Resp} ->
            ok = k_j3_support:block_request(ReqId),
            {ok, <<>>};
        {error, no_entry} ->
            {http_code, 404}
    end.

update(_Params) ->
    ok.

delete(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_statistic_mt_batches:get_details(ReqId) of
        {ok, _Resp} ->
            ok = k_j3_support:unblock_request(ReqId),
            {ok, <<>>};
        {error, no_entry} ->
            {http_code, 404}
    end.
