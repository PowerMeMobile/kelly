-module(k_storage_networks).

%% API
-export([
    set_network/2,
    get_network/1,
    get_networks/0,
    del_network/1
]).

-include("storages.hrl").
-include("network.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_network(network_id(), #network{}) -> ok | {error, term()}.
set_network(NetworkId, Network)->
    Modifier = {
        '$set', {
            'name'        , Network#network.name,
            'country'     , Network#network.country,
            'hex_code'    , Network#network.hex_code,
            'country_code', Network#network.country_code,
            'number_len'  , Network#network.number_len,
            'prefixes'    , Network#network.prefixes,
            'gmt_diff'    , Network#network.gmt_diff,
            'dst'         , Network#network.dst,
            'provider_id' , Network#network.provider_id,
            'is_home'     , Network#network.is_home,
            'sms_points'  , Network#network.sms_points,
            'sms_mult_points', Network#network.sms_mult_points
        }
    },
    case mongodb_storage:upsert(static_storage, networks, {'_id', NetworkId}, Modifier) of
        ok ->
            ok = k_event_manager:notify_network_changed(NetworkId);
        {error, Error} ->
            {error, Error}
    end.

-spec get_network(network_id()) -> {ok, #network{}} | {error, no_entry} | {error, term()}.
get_network(NetworkId) ->
    case mongodb_storage:find_one(static_storage, networks, {'_id', NetworkId}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec get_networks() -> {ok, [#network{}]} | {error, term()}.
get_networks() ->
    case mongodb_storage:find(static_storage, networks, {}) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        Error ->
            Error
    end.

-spec del_network(network_id()) -> ok | {error, no_entry} | {error, term()}.
del_network(NetworkId) ->
    case mongodb_storage:delete(static_storage, networks, {'_id', NetworkId}) of
        ok ->
            ok = k_event_manager:notify_network_changed(NetworkId);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    Id = bsondoc:at('_id', Doc),
    Name = bsondoc:at(name, Doc),
    Country = bsondoc:at(country, Doc),
    HexCode = bsondoc:at(hex_code, Doc),
    CountryCode = bsondoc:at(country_code, Doc),
    NumberLen = bsondoc:at(number_len, Doc),
    Prefixes = bsondoc:at(prefixes, Doc),
    GMTDiff = bsondoc:at(gmt_diff, Doc),
    DST = bsondoc:at(dst, Doc),
    ProviderId = bsondoc:at(provider_id, Doc),
    IsHome = bsondoc:at(is_home, Doc),
    SmsPoints = bsondoc:at(sms_points, Doc),
    SmsMultPoints = bsondoc:at(sms_mult_points, Doc),
    #network{
        id = Id,
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
    }.
