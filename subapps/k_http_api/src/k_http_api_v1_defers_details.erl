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
        #param{name = def_date, mandatory = false, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = message, mandatory = false, repeated = false, type = binary}
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
    _ReqId = ?gv(req_id, Params),
    %% The message language can not be changed. (different encoding)
    %% Deferred date and time can not be set in the past. Please specify deferred date and time at least one minute past the current time (03/03/2015 16:55:41). (should be checked on client as well)
    %% Number of messages should not be changed. Original size was 2 messages, but now it is 1 (different lenght)
    {ok, [{defers_details, put}]}.

delete(Params) ->
    ReqId = ?gv(req_id, Params),
    ok = k_storage_defers:delete(ReqId),
    {http_code, 204}.
