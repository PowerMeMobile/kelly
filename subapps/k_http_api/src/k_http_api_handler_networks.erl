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
        #param{name = sms_mult_credits, mandatory = false, repeated = false, type = float}
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
        #param{name = sms_mult_credits, mandatory = true, repeated = false, type = float}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/networks/[:id]"
    }}.

read(Params) ->
    NetworkUUID = ?gv(id, Params),
    case NetworkUUID of
        undefined ->
            read_all();
        _ -> read_id(NetworkUUID)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            UUID = uuid:unparse(uuid:generate_time()),
            create_network(lists:keyreplace(id, 1, Params, {id, UUID}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    ID = ?gv(id, Params),
    case k_config:get_network(ID) of
        {ok, Network = #network{}} ->
            update_network(Network, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    NetworkId = ?gv(id, Params),
    ok = k_config:del_network(NetworkId),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

read_all() ->
    case k_config:get_networks() of
        {ok, NtwList} ->
            {ok, NtwPropLists} = prepare(NtwList),
            ?log_debug("NtwPropLists: ~p", [NtwPropLists]),
            {http_code, 200, {networks, NtwPropLists}};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500};
        Error ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(NtwUUID) ->
    case k_config:get_network(NtwUUID) of
        {ok, Ntw = #network{}} ->
            {ok, [NtwPropList]} = prepare({NtwUUID, Ntw}),
            ?log_debug("NtwPropList: ~p", [NtwPropList]),
            {http_code, 200, NtwPropList};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

is_exist(Params) ->
    UUID = ?gv(id, Params),
    case k_config:get_network(UUID) of
        {ok, #network{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_network(Params)
    end.

update_network(Network, Params) ->
    ID = ?gv(id, Params),
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
    SmsMultCredits = ?gv(sms_mult_credits, Params, Network#network.sms_mult_credits),
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
        sms_mult_credits = SmsMultCredits
    },
    ok = k_config:set_network(ID, Updated),
    {ok, [NtwPropList]} = prepare({ID, Updated}),
    ?log_debug("NtwPropList: ~p", [NtwPropList]),
    {http_code, 200, NtwPropList}.

create_network(Params) ->
    ID = ?gv(id, Params),
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
    SmsMultCredits = ?gv(sms_mult_credits, Params),
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
        sms_mult_credits = SmsMultCredits
    },
    ok = k_config:set_network(ID, Network),
    {ok, [NtwPropList]} = prepare({ID, Network}),
    ?log_debug("NtwPropList: ~p", [NtwPropList]),
    {http_code, 201, NtwPropList}.

prepare(NtwList) when is_list(NtwList) ->
    prepare(NtwList, []);
prepare(Ntw = {_UUID, #network{}}) ->
    prepare([Ntw]).

prepare([], Acc) ->
    {ok, Acc};
prepare([{NtwUUID, Ntw = #network{}} | Rest], Acc) ->
    NtwFun = ?record_to_proplist(network),
    PropList = NtwFun(Ntw),
    Name = ?gv(name, PropList),
    Country = ?gv(country, PropList),
    HexCode = ?gv(hex_code, PropList),
    CountryCode = ?gv(country_code, PropList),
    NumberLen = ?gv(number_len, PropList),
    Prefixes = ?gv(prefixes, PropList),
    GMTDiff = ?gv(gmt_diff, PropList),
    DST = ?gv(dst, PropList),
    ProviderId = ?gv(provider_id, PropList),
    IsHome = ?gv(is_home, PropList),
    SmsPoints = ?gv(sms_points, PropList),
    SmsMultCredits = ?gv(sms_mult_credits, PropList),
    Result = [
        {id, NtwUUID},
        {name, Name},
        {country, Country},
        {hex_code, HexCode},
        {country_code, CountryCode},
        {number_len, NumberLen},
        {prefixes, Prefixes},
        {gmt_diff, GMTDiff},
        {dst, DST},
        {provider_id, ProviderId},
        {is_home, IsHome},
        {sms_points, SmsPoints},
        {sms_mult_credits, SmsMultCredits}
    ],
    prepare(Rest, [Result | Acc]).
