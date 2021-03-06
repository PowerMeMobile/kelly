-module(k_http_api_v1_blacklist).

-behaviour(gen_http_api).

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
-include_lib("k_storage/include/blacklist_entry.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = id, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = id, mandatory = true, repeated = false, type = binary},
        #param{name = dst_addr, mandatory = false, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}},
        #param{name = src_addr, mandatory = false, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = dst_addr, mandatory = true, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}},
        #param{name = src_addr, mandatory = false, repeated = false, type =
            {custom, fun k_http_api_utils:decode_msisdn/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/v1/blacklist/[:id]"
    }}.

read(Params) ->
    Uuid = ?gv(id, Params),
    case Uuid of
        undefined -> read_all();
        _ -> read_id(Uuid)
    end.

create(Params) ->
    case ?gv(id, Params) of
        undefined ->
            Uuid = uuid:unparse(uuid:generate_time()),
            create_blacklist_entry(lists:keyreplace(id, 1, Params, {id, Uuid}));
        _ ->
            is_exist(Params)
    end.

update(Params) ->
    case k_storage_blacklist:get_blacklist_entry(?gv(id, Params)) of
        {ok, Entry = #blacklist_entry{}} ->
            update_blacklist_entry(Entry, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    ok = k_storage_blacklist:del_blacklist_entry(?gv(id, Params)),
    {http_code, 204}.

%% ===================================================================
%% Internal
%% ===================================================================

is_exist(Params) ->
    case k_storage_blacklist:get_blacklist_entry(?gv(id, Params)) of
        {ok, #blacklist_entry{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_blacklist_entry(Params)
    end.

read_all() ->
    case k_storage_blacklist:get_blacklist_entries() of
        {ok, Entries} ->
            {ok, Plists} = prepare(Entries),
            ?log_debug("Blacklist Entries: ~p", [Plists]),
            {http_code, 200, Plists}
    end.

read_id(Uuid) ->
    case k_storage_blacklist:get_blacklist_entry(Uuid) of
        {ok, Entry = #blacklist_entry{}} ->
            {ok, [Plist]} = prepare(Entry),
            ?log_debug("Blacklist Entry: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003', []}
    end.

update_blacklist_entry(Entry, Params) ->
    Id = ?gv(id, Params),
    DstAddr = ?gv(dst_addr, Params, Entry#blacklist_entry.dst_addr),
    SrcAddr = ?gv(src_addr, Params),
    Updated = #blacklist_entry{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    },
    ok = k_storage_blacklist:set_blacklist_entry(Id, Updated),
    {ok, [Plist]} = prepare(Updated),
    ?log_debug("Blacklist Entry: ~p", [Plist]),
    {http_code, 200, Plist}.

create_blacklist_entry(Params) ->
    Id = ?gv(id, Params),
    DstAddr = ?gv(dst_addr, Params),
    SrcAddr = ?gv(src_addr, Params),
    Entry = #blacklist_entry{
        id = Id,
        dst_addr = DstAddr,
        src_addr = SrcAddr
    },
    ok = k_storage_blacklist:set_blacklist_entry(Id, Entry),
    {ok, [Plist]} = prepare(Entry),
    ?log_debug("Blacklist Entry: ~p", [Plist]),
    {http_code, 201, Plist}.

prepare(List) when is_list(List) ->
    prepare(List, []);
prepare(Entry = #blacklist_entry{}) ->
    prepare([Entry]).

prepare([], Acc) ->
    {ok, Acc};
prepare([Entry = #blacklist_entry{} | Rest], Acc) ->
    AddrFun = ?record_to_proplist(addr),
    SafeAddrFun =
        fun(undefined) -> undefined;
           (Addr) -> AddrFun(Addr)
        end,
    EntryFun = ?record_to_proplist(blacklist_entry),
    DstAddrPlist = SafeAddrFun(Entry#blacklist_entry.dst_addr),
    SrcAddrPlist = SafeAddrFun(Entry#blacklist_entry.src_addr),
    Plist = EntryFun(
        Entry#blacklist_entry{
            dst_addr = DstAddrPlist,
            src_addr = SrcAddrPlist
        }
    ),
    prepare(Rest, [Plist | Acc]).
