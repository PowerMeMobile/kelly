-module(k_http_api_handler_originators).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

%% export helpers
-export([
    prepare_originators/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = false, repeated = false, type = integer}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = integer},
        #param{name = address, mandatory = false, repeated = false, type =
            {custom, fun decode_addr/1}},
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = is_default, mandatory = false, repeated = false, type = boolean},
        #param{name = state, mandatory = false, repeated = false, type =
            {custom, fun originator_state/1}}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = integer}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = id, mandatory = true, repeated = false, type = integer},
        #param{name = address, mandatory = true, repeated = false, type =
            {custom, fun decode_addr/1}},
        #param{name = description, mandatory = false, repeated = false, type = binary},
        #param{name = is_default, mandatory = false, repeated = false, type = boolean},
        #param{name = state, mandatory = true, repeated = false, type =
            {custom, fun originator_state/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/customers/:customer_uuid/originators/[:id]"
    }}.

create(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            create_originator(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            get_customer_originator(Customer, ?gv(id, Params));
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, Customer = #customer{}} ->
            update_originator(Customer, Params);
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

delete(Params) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    OriginatorId = ?gv(id, Params),
    case k_storage_customers:del_customer_originator(CustomerUuid, OriginatorId) of
        {error, no_entry} ->
            ?log_warn("Customer [~p] not found", [CustomerUuid]),
            {exception, 'svc0003'};
        ok ->
            {http_code, 204};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

-spec prepare_originators(#originator{}) -> {ok, [{atom(), term()}]}.
prepare_originators(Originator = #originator{}) ->
    AddrFun = ?record_to_proplist(addr),
    OrigFun = ?record_to_proplist(originator),
    AddrPlist = AddrFun(Originator#originator.address),
    Originator2 = Originator#originator{address = AddrPlist},
    OrigFun(Originator2);
prepare_originators(Originators) when is_list(Originators) ->
    {ok, [prepare_originators(Originator) || Originator <- Originators]}.

%% ===================================================================
%% Local Functions
%% ===================================================================

create_originator(Customer, Params) ->
    Id = ?gv(id, Params),
    case k_storage_customers:get_customer_originator(Customer, Id) of
        {ok, #originator{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            Address = ?gv(address, Params),
            Description = ?gv(description, Params),
            IsDefault = ?gv(is_default, Params),
            State = ?gv(state, Params),
            Originator = #originator{
                id = Id,
                address = Address,
                description = Description,
                is_default = IsDefault,
                state = State
            },
            ok = k_storage_customers:set_customer_originator(Originator, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_originators([Originator]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 201, Plist};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update_originator(Customer, Params) ->
    Id = ?gv(id, Params),
    case k_storage_customers:get_customer_originator(Customer, Id) of
        {ok, Originator} ->
            Address = ?gv(address, Params, Originator#originator.address),
            Description = ?gv(description, Params, Originator#originator.description),
            IsDefault = ?gv(is_default, Params, Originator#originator.is_default),
            State = ?gv(state, Params, Originator#originator.state),
            Updated = #originator{
                id = Id,
                address = Address,
                description = Description,
                is_default = IsDefault,
                state = State
            },
            ok = k_storage_customers:set_customer_originator(Updated, Customer#customer.customer_uuid),
            {ok, [Plist]} = prepare_originators([Updated]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

get_customer_originator(Customer, undefined) ->
    #customer{originators = Originators} = Customer,
    {ok, Plist} = prepare_originators(Originators),
    ?log_debug("Originator: ~p", [Plist]),
    {http_code, 200, Plist};
get_customer_originator(Customer, Id) ->
    case k_storage_customers:get_customer_originator(Customer, Id) of
        {ok, Originator} ->
            {ok, [Plist]} = prepare_originators([Originator]),
            ?log_debug("Originator: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

%% convert "addr,ton,npi" to #addr{addr, ton, npi}
decode_addr(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.

originator_state(State) ->
    case State of
        <<"pending">> -> pending;
        <<"approved">> -> approved;
        <<"rejected">> -> rejected
    end.

