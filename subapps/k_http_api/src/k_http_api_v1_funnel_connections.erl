-module(k_http_api_v1_funnel_connections).

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
    {ok, #specs{
        read = [],
        delete = [
            #param{name = customer_id, mandatory = false, repeated = false, type = binary},
            #param{name = user_id, mandatory = false, repeated = false, type = binary},
            #param{name = bind_type, mandatory = false, repeated = true, type = {custom, fun bind_type/1}},
            #param{name = connection_id, mandatory = false, repeated = true, type = uuid}
        ],
        route = "/v1/funnel/connections"
    }}.

read(_Params) ->
    case k_control_funnel:connections() of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Get funnel connections failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(Params) ->
    CustomerId = ?gv(customer_id, Params),
    UserId = ?gv(user_id, Params),
    BindType = ?gv(bind_type, Params),
    ConnectionId = ?gv(connection_id, Params),
    case k_control_funnel:disconnect(
            CustomerId, UserId, BindType, ConnectionId) of
        ok ->
            {ok, <<>>};
        {error, Error} ->
            ?log_debug(
                "Funnel disconnect "
                "(customer_id: ~p,  user_id: ~p, bind_type: ~p connection_id: ~p) "
                "failed with: ~p",
                [CustomerId, UserId, BindType, ConnectionId, Error]),
            {exception, 'svc0003'}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

bind_type(TypeBin) ->
    case TypeBin of
        <<"transmitter">> -> transmitter;
        <<"receiver">> -> receiver;
        <<"transceiver">> -> transceiver
    end.
