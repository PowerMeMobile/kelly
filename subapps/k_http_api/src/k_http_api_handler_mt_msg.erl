-module(k_http_api_handler_mt_msg).

-behaviour(gen_http_api).

%% gen_cowboy_crud callbacks
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
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = msg_id, mandatory = true, repeated = false, type = binary}
    ],
    {ok, #specs{
        read = Read,
        route = "/report/mt_msg"
    }}.

read(Params) ->
    MsgId = ?gv(msg_id, Params),
    Report =
        try k_storage_utils:binary_to_objectid(MsgId) of
            _ ->
                k_statistic_mt_messages:build_msg_report(MsgId)
        catch
            _:_ ->
                k_statistic_mo_messages:build_msg_report(MsgId)
        end,
    {ok, Report}.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
