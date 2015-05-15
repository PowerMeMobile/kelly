-module(k_storage_providers).

%% API
-export([
    set_provider/2,
    get_provider/1,
    get_providers/0,
    del_provider/1
]).

-include("storages.hrl").
-include("provider.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec set_provider(provider_id(), #provider{}) -> ok | {error, term()}.
set_provider(ProviderId, Provider)->
    Modifier = {
        '$set' , {
            'name'              , Provider#provider.name,
            'description'       , Provider#provider.description,
            'gateway_id'        , Provider#provider.gateway_id,
            'bulk_gateway_id'   , Provider#provider.bulk_gateway_id,
            'receipts_supported', Provider#provider.receipts_supported,
            'sms_add_points'    , Provider#provider.sms_add_points
        }
    },
    case mongodb_storage:upsert(static_storage, providers, {'_id', ProviderId}, Modifier) of
        ok ->
            ok = k_event_manager:notify_provider_changed(ProviderId);
        {error, Error} ->
            {error, Error}
    end.

-spec get_provider(provider_id()) -> {ok, #provider{}} | {error, no_entry} | {error, term()}.
get_provider(ProviderId) ->
    case mongodb_storage:find_one(static_storage, providers, {'_id', ProviderId}) of
        {ok, Doc} ->
            {ok, doc_to_record(Doc)};
        Error ->
            Error
    end.

-spec get_providers() -> {ok, [#provider{}]} | {error, term()}.
get_providers() ->
    case mongodb_storage:find(static_storage, providers, {}) of
        {ok, List} ->
            {ok, [doc_to_record(Doc) || {_Id, Doc} <- List]};
        Error ->
            Error
    end.

-spec del_provider(provider_id()) -> ok | {error, no_entry} | {error, term()}.
del_provider(ProviderId) ->
    case mongodb_storage:delete(static_storage, providers, {'_id', ProviderId}) of
        ok ->
            ok = k_event_manager:notify_provider_changed(ProviderId);
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internals
%% ===================================================================

doc_to_record(Doc) ->
    Id = bsondoc:at('_id', Doc),
    Name = bsondoc:at(name, Doc),
    Description = bsondoc:at(description, Doc),
    GtwId = bsondoc:at(gateway_id, Doc),
    BulkGtwId = bsondoc:at(bulk_gateway_id, Doc),
    ReceiptsSupported = bsondoc:at(receipts_supported, Doc),
    SmsAddPoints = bsondoc:at(sms_add_points, Doc),
    #provider{
        id = Id,
        name = Name,
        description = Description,
        gateway_id = GtwId,
        bulk_gateway_id = BulkGtwId,
        receipts_supported = ReceiptsSupported,
        sms_add_points = SmsAddPoints
    }.
