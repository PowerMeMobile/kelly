-module(k_http_api_handler_just_block).

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
        route = "/just/block/:req_id"
    }}.

read(_Params) ->
    ok.

create(Params) ->
    ReqId = ?gv(req_id, Params),
    Result = k_j3_support:block_request(ReqId),
    {ok, {result, Result}}.

update(_Params) ->
    ok.

delete(Params) ->
    ReqId = ?gv(req_id, Params),
    Result = k_j3_support:unblock_request(ReqId),
    {ok, {result, Result}}.
