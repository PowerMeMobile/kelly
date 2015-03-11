-module(k_http_api_v1_defers_details).

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
    Update = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid},
        #param{name = def_time, mandatory = false, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = body, mandatory = false, repeated = false, type = binary}
    ],
    Delete = [
        #param{name = req_id, mandatory = true, repeated = false, type = uuid}
    ],
    {ok, #specs{
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/defers/:req_id"
    }}.

read(Params) ->
    ReqId = ?gv(req_id, Params),
    case k_defers:get_details(ReqId) of
        {ok, Resp} ->
            {ok, Resp};
        {error, no_entry} ->
            {http_code, 404}
    end.

create(_Params) ->
    ok.

update(Params) ->
    case k_defers:update(Params) of
        ok ->
            {ok, <<>>};
        {error, no_entry} ->
            {http_code, 404};
        {error, {Error, Was, Now}} ->
            {http_code, 403, [{error, Error}, {was, Was}, {now, Now}]};
        {error, Error} ->
            {http_code, 403, [{error, Error}]}
    end.

delete(Params) ->
    ReqId = ?gv(req_id, Params),
    ok = k_storage_defers:delete(ReqId),
    {http_code, 204}.
