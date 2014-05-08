-module(k_http_api_handler_blacklist).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("k_common/include/logging.hrl").
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
            {custom, fun decode_addr/1}},
        #param{name = src_addr, mandatory = false, repeated = false, type =
            {custom, fun decode_addr/1}}
    ],
    Delete = [
        #param{name = id, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = id, mandatory = false, repeated = false, type = binary},
        #param{name = dst_addr, mandatory = true, repeated = false, type =
            {custom, fun decode_addr/1}},
        #param{name = src_addr, mandatory = false, repeated = false, type =
            {custom, fun decode_addr/1}}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/blacklist/[:id]"
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
            {exception, 'svc0003'};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

delete(Params) ->
    ok = k_storage_blacklist:del_blacklist_entry(?gv(id, Params)),
    {http_code, 204}.

%% ===================================================================
%% Local Functions
%% ===================================================================

is_exist(Params) ->
    case k_storage_blacklist:get_blacklist_entry(?gv(id, Params)) of
        {ok, #blacklist_entry{}} ->
            {exception, 'svc0004'};
        {error, no_entry} ->
            create_blacklist_entry(Params);
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_all() ->
    case k_storage_blacklist:get_blacklist_entries() of
        {ok, Entries} ->
            {ok, Plists} = prepare(Entries),
            ?log_debug("Blacklist Entries: ~p", [Plists]),
            {http_code, 200, Plists};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

read_id(Uuid) ->
    case k_storage_blacklist:get_blacklist_entry(Uuid) of
        {ok, Entry = #blacklist_entry{}} ->
            {ok, [Plist]} = prepare(Entry),
            ?log_debug("Blacklist Entry: ~p", [Plist]),
            {http_code, 200, Plist};
        {error, no_entry} ->
            {exception, 'svc0003', []};
        {error, Error} ->
            ?log_error("Unexpected error: ~p", [Error]),
            {http_code, 500}
    end.

update_blacklist_entry(Entry, Params) ->
    Uuid = ?gv(id, Params),
    DstAddr = ?gv(dst_addr, Params, Entry#blacklist_entry.dst_addr),
    SrcAddr = ?gv(src_addr, Params, Entry#blacklist_entry.src_addr),
    Updated = #blacklist_entry{
        dst_addr = DstAddr,
        src_addr = SrcAddr
    },
    ok = k_storage_blacklist:set_blacklist_entry(Uuid, Updated),
    {ok, [Plist]} = prepare(Updated),
    ?log_debug("Blacklist Entry: ~p", [Plist]),
    {http_code, 200, Plist}.

create_blacklist_entry(Params) ->
    Uuid = ?gv(id, Params),
    DstAddr = ?gv(dst_addr, Params),
    SrcAddr = ?gv(src_addr, Params),
    Entry = #blacklist_entry{
        dst_addr = DstAddr,
        src_addr = SrcAddr
    },
    ok = k_storage_blacklist:set_blacklist_entry(Uuid, Entry),
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

%% convert "addr,ton,npi" to #addr{addr, ton, npi}
decode_addr(<<>>) ->
    undefined;
decode_addr(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.
