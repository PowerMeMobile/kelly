-module(k_http_api_handler_networks).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/network.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = id, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = false, repeated = false, type = binary},
        #param{name = country, mandatory = false, repeated = false, type = binary},
        #param{name = hex_code, mandatory = false, repeated = false, type = binary},
        #param{name = country_code, mandatory = false, repeated = false, type = binary},
        #param{name = number_len,  mandatory = false, repeated = false, type = integer},
        #param{name = prefixes, mandatory = false, repeated = true, type = binary},
        #param{name = gmt_diff, mandatory = false, repeated = false, type = binary},
        #param{name = dst, mandatory = false, repeated = false, type = binary},
        #param{name = provider_id, mandatory = false, repeated = false, type = binary},
        #param{name = is_home, mandatory = false, repeated = false, type = boolean},
        #param{name = sms_points, mandatory = false, repeated = false, type = float},
        #param{name = sms_mult_points, mandatory = false, repeated = false, type = float}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = country, mandatory = true, repeated = false, type = binary},
        #param{name = hex_code, mandatory = true, repeated = false, type = binary},
        #param{name = country_code, mandatory = true, repeated = false, type = binary},
        #param{name = number_len,  mandatory = true, repeated = false, type = integer},
        #param{name = prefixes, mandatory = true, repeated = true, type = binary},
        #param{name = gmt_diff, mandatory = true, repeated = false, type = binary},
        #param{name = dst, mandatory = true, repeated = false, type = binary},
        #param{name = provider_id, mandatory = true, repeated = false, type = binary},
        #param{name = is_home, mandatory = true, repeated = false, type = boolean},
        #param{name = sms_points, mandatory = true, repeated = false, type = float},
        #param{name = sms_mult_points, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/networks/[:id]"
    }}.

read(Params) ->
    Uuid = ?gv(id, Params),
    case Uuid of
        undefined ->
            read_all();
        _ ->
            read_id(Uuid)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            Uuid = uuid:unparse(uuid:generate_time()),
            create_network(lists:keyreplace(id, 1, Params, {id, Uuid}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    ID = ?gv(id, Params),
    case k_storage_networks:get_network(ID) of
        {ok, Entry = #network{}} ->
            update_network(Entry, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    Uuid = ?gv(id, Params),
    ok = k_storage_networks:del_network(Uuid),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
    case k_storage_networks:get_networks() of
        {ok, Entries} ->
            {ok, Plists} = prepare(Entries),
            ?log_debug("Networks: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(Uuid) ->
    case k_storage_networks:get_network(Uuid) of
        {ok, Entry = #network{}} ->
            {ok, [Plist]} = prepare(Entry),
            ?log_debug("Network: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

is_exist(Params) ->
    Uuid = ?gv(id, Params),
    case k_storage_networks:get_network(Uuid) of
        {ok, #network{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_network(Params)
    end.

update_network(Network, Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params, Network#network.name),
    Country = ?gv(country, Params, Network#network.country),
    HexCode = ?gv(hex_code, Params, Network#network.hex_code),
    CountryCode = ?gv(country_code, Params, Network#network.country_code),
    NumberLen = ?gv(number_len, Params, Network#network.number_len),
    Prefixes = ?gv(prefixes, Params, Network#network.prefixes),
    GMTDiff = ?gv(gmt_diff, Params, Network#network.gmt_diff),
    DST = ?gv(dst, Params, Network#network.dst),
    ProviderId = ?gv(provider_id, Params, Network#network.provider_id),
    IsHome = ?gv(is_home, Params, Network#network.is_home),
    SmsPoints = ?gv(sms_points, Params, Network#network.sms_points),
    SmsMultPoints = ?gv(sms_mult_points, Params, Network#network.sms_mult_points),
    Updated = #network{
        name = Name,
        country = Country,
        hex_code = HexCode,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        gmt_diff = GMTDiff,
        dst = DST,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    },
    ok = k_storage_networks:set_network(Id, Updated),
    {ok, [Plist]} = prepare(Updated),
    ?log_debug("Network: ~p", [Plist]),
    {http_code, 200, Plist}.

create_network(Params) ->
    Id = ?gv(id, Params),
    Name = ?gv(name, Params),
    Country = ?gv(country, Params),
    HexCode = ?gv(hex_code, Params),
    CountryCode = ?gv(country_code, Params),
    NumberLen = ?gv(number_len, Params),
    Prefixes = ?gv(prefixes, Params),
    GMTDiff = ?gv(gmt_diff, Params),
    DST = ?gv(dst, Params),
    ProviderId = ?gv(provider_id, Params),
    IsHome = ?gv(is_home, Params),
    SmsPoints = ?gv(sms_points, Params),
    SmsMultPoints = ?gv(sms_mult_points, Params),
    Network = #network{
        name = Name,
        country = Country,
        hex_code = HexCode,
        country_code = CountryCode,
        number_len = NumberLen,
        prefixes = Prefixes,
        gmt_diff = GMTDiff,
        dst = DST,
        provider_id = ProviderId,
        is_home = IsHome,
        sms_points = SmsPoints,
        sms_mult_points = SmsMultPoints
    },
    ok = k_storage_networks:set_network(Id, Network),
    {ok, [Plist]} = prepare(Network),
    ?log_debug("Network: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare(List) when is_list(List) ->
    prepare(List, []);
prepare(Entry = #network{}) ->
    prepare([Entry]).

prepare([], Acc) ->
    {ok, Acc};
prepare([Ntw = #network{} | Rest], Acc) ->
    Fun = ?record_to_proplist(network),
    Plist = Fun(Ntw),
    prepare(Rest, [Plist | Acc]).
