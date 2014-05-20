-module(k_http_api_handler_message_status).

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
-include_lib("k_storage/include/msg_info.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
    Read = [
        #param{name = message_id, mandatory = true, repeated = false, type = binary},
        #param{name = client_type, mandatory = true, repeated = false, type = atom},
        #param{name = customer_id, mandatory = true, repeated = false, type = binary},
        #param{name = user_id, mandatory = true, repeated = false, type = binary}
    ],
    {ok, #specs{
        create = undefined,
        read = Read,
        update = undefined,
        delete = undefined,
        route = "/message_status/:message_id/client/:client_type/customer/:customer_id/user/:user_id"
    }}.

read(Params) ->
    ?log_debug("Params: ~p", [Params]),
    CustomerId = ?gv(customer_id, Params),
    UserId = ?gv(user_id, Params),
    ClientType = ?gv(client_type, Params),
    InMsgId = ?gv(message_id, Params),
    case k_statistic:get_mt_msg_status_report(CustomerId, UserId, ClientType, InMsgId) of
        {ok, Report} ->
            {ok, Report};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.
