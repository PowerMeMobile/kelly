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
    Update = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    Delete = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        create = undefined,
        read = undefined,
        update = Update,
        delete = Delete,
        route = "/v1/batches/:req_id/block"
    }}.

read(_Params) ->
    ok.

create(_Params) ->
    ok.

update(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_dynamic_storage:set_mt_batch_status(ReqId, blocked) of
        ok ->
            ok = k_control_just:block_request(ReqId),
            {ok, <<>>};
        {error, no_entry} ->
            {http_code, 404}
    end.

delete(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_dynamic_storage:set_mt_batch_status(ReqId, unblocked) of
        ok ->
            ok = k_control_just:unblock_request(ReqId),
            {ok, <<>>};
        {error, no_entry} ->
            {http_code, 404}
    end.
