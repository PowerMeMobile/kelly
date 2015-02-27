-module(k_http_api_handler_statuses_stats).

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
        #param{name = from, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        #param{name = to, mandatory = true, repeated = false, type =
            {custom, fun ac_datetime:iso8601_to_datetime/1}},
        %% customer_id is deprecated, force clients to use customer_uuid
        #param{name = customer_id, mandatory = false, repeated = false, type = uuid},
        #param{name = customer_uuid, mandatory = false, repeated = false, type = uuid},
        #param{name = status, mandatory = false, repeated = false, type = atom}
    ],
    {ok, #specs{
        read = Read,
        route = "/report/statuses"
    }}.

read(Params) ->
    ?log_debug("Params: ~p", [Params]),
    From = ?gv(from, Params),
    To = ?gv(to, Params),
    CustomerUuid = get_customer_uuid(Params),
    Status = ?gv(status, Params),
    case build_report(From, To, CustomerUuid, Status) of
        {ok, Report} ->
            {ok, Report};
        {error, Error} ->
            ?log_debug("Statuses report failed with: ~p", [Error]),
            {exception, 'svc0003'}
    end.

create(_Params) ->
    ok.

update(_Params) ->
    ok.

delete(_Params) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

build_report(From, To, CustomerUuid, undefined) ->
    k_statistic:get_aggregated_statuses_report(From, To, CustomerUuid);

build_report(From, To, CustomerUuid, Status) ->
    k_statistic:get_msgs_by_status_report(From, To, CustomerUuid, Status).

get_customer_uuid(Params) ->
    case ?gv(customer_uuid, Params) of
        undefined ->
            ?gv(customer_id, Params);
        CustomerUuid ->
            CustomerUuid
    end.
