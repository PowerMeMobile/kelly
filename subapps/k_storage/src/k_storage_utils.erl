-module(k_storage_utils).

-export([
    addr_to_doc/1,
    doc_to_addr/1,

    addr_to_proplist/1,

    doc_to_mt_msg_info/1,
    doc_to_mt_batch_info/1,

    doc_to_mo_msg_info/1,

    objectid_to_binary/1,
    binary_to_objectid/1,

    encoding_to_binary/1,
    binary_to_encoding/1,

    get_uuid_to_customer_dict/1,
    get_id_to_gateway_dict/1,
    get_id_to_network_dict/1,
    get_id_to_provider_dict/1
]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

-include("msg_info.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec addr_to_doc(#addr{}) -> {a, binary(), t, integer(), n, integer()}.
addr_to_doc(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
    {a, Addr, t, Ton, n, Npi};
addr_to_doc(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
    {a, Addr, t, Ton, n, Npi, r, RefNum}.

-spec doc_to_addr
    ({a, binary(), t, integer(), n, integer()}) ->
        #addr{};
    ({a, binary(), t, integer(), n, integer(), r, integer()}) ->
        #addr{}.
doc_to_addr({a, Addr, t, Ton, n, Npi}) ->
    #addr{
        addr = Addr,
        ton = Ton,
        npi = Npi
    };
doc_to_addr({a, Addr, t, Ton, n, Npi, r, RefNum}) ->
    #addr{
        addr = Addr,
        ton = Ton,
        npi = Npi,
        ref_num = RefNum
    }.

-spec addr_to_proplist(#addr{}) -> [{addr, binary()} | {ton | npi | ref_num, integer()}].
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi}
    ];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
    [
        {addr, Addr},
        {ton, Ton},
        {npi, Npi},
        {ref_num, RefNum}
    ].

-spec doc_to_mt_msg_info(bson:document()) -> #msg_info{}.
doc_to_mt_msg_info(Doc) ->
    ObjId = bsondoc:at('_id', Doc),
    Type =
        case bsondoc:at(t, Doc) of
            <<"regular">> ->
                regular;
            {n, <<"part">>, r, PartRef, s, PartSeq, t, PartsTotal} ->
                {part, #part_info{ref = PartRef, seq = PartSeq, total = PartsTotal}}
        end,
    #msg_info{
        msg_id = objectid_to_binary(ObjId),
        client_type = bsondoc:binary_to_atom(bsondoc:at(ct, Doc)),
        customer_uuid = bsondoc:at(ci, Doc),
        user_id = bsondoc:at(ui, Doc),
        req_id = bsondoc:at(ri, Doc),
        in_msg_id = bsondoc:at(imi, Doc),
        gateway_id = bsondoc:at(gi, Doc),
        out_msg_id = bsondoc:at(omi, Doc),
        type = Type,
        src_addr = doc_to_addr(bsondoc:at(sa, Doc)),
        dst_addr = doc_to_addr(bsondoc:at(da, Doc)),
        encoding = binary_to_encoding(bsondoc:at(e, Doc)),
        body = bsondoc:at(b, Doc),
        reg_dlr = bsondoc:at(rd, Doc),
        esm_class = bsondoc:at(ec, Doc),
        val_period = bsondoc:at(vp, Doc),
        status = bsondoc:binary_to_atom(bsondoc:at(s, Doc)),
        req_time = bsondoc:at(rqt, Doc),
        resp_time = bsondoc:at(rpt, Doc),
        dlr_time = bsondoc:at(dt, Doc),
        network_id = bsondoc:at(ni, Doc),
        price = bsondoc:at(p, Doc)
    }.

-spec doc_to_mt_batch_info(bson:document()) -> #batch_info{}.
doc_to_mt_batch_info(Doc) ->
    #batch_info{
        req_id = bsondoc:at('_id', Doc),
        customer_uuid = bsondoc:at(ci, Doc),
        user_id = bsondoc:at(ui, Doc),
        client_type = bsondoc:binary_to_atom(bsondoc:at(ct, Doc)),
        def_time = bsondoc:at(dft, Doc),
        req_type = bsondoc:binary_to_atom(bsondoc:at(rt, Doc)),
        src_addr = doc_to_addr(bsondoc:at(sa, Doc)),
        encoding = binary_to_encoding(bsondoc:at(e, Doc)),
        body = bsondoc:at(b, Doc),
        reg_dlr = bsondoc:at(rd, Doc),
        esm_class = bsondoc:at(ec, Doc),
        val_period = bsondoc:at(vp, Doc),
        status = bsondoc:binary_to_atom(bsondoc:at(s, Doc)),
        req_time = bsondoc:at(rqt, Doc),
        recipients = bsondoc:at(rs, Doc),
        messages = bsondoc:at(ms, Doc),
        price = bsondoc:at(p, Doc)
    }.

-spec doc_to_mo_msg_info(bson:document()) -> #msg_info{}.
doc_to_mo_msg_info(Doc) ->
    #msg_info{
        msg_id = bsondoc:at('_id', Doc),
        customer_uuid = bsondoc:at(ci, Doc),
        user_id = bsondoc:at(ui, Doc),
        gateway_id = bsondoc:at(gi, Doc),
        type = bsondoc:binary_to_atom(bsondoc:at(t, Doc)),
        encoding = binary_to_encoding(bsondoc:at(e, Doc)),
        body = bsondoc:at(b, Doc),
        src_addr = doc_to_addr(bsondoc:at(sa, Doc)),
        dst_addr = doc_to_addr(bsondoc:at(da, Doc)),
        reg_dlr = bsondoc:at(rd, Doc),
        status = bsondoc:binary_to_atom(bsondoc:at(s, Doc)),
        req_time = bsondoc:at(rqt, Doc)
    }.

-spec objectid_to_binary(bson:objectid()) -> binary().
objectid_to_binary({ObjId}) ->
    ac_hexdump:binary_to_hexdump(ObjId, to_lower).

-spec binary_to_objectid(binary()) -> bson:objectid().
binary_to_objectid(Bin) ->
    {ac_hexdump:hexdump_to_binary(Bin)}.

-spec encoding_to_binary(atom() | integer()) -> binary().
encoding_to_binary(Encoding) when is_atom(Encoding) ->
    bsondoc:atom_to_binary(Encoding);
encoding_to_binary(Encoding) when is_integer(Encoding) ->
    integer_to_binary(Encoding).

-spec binary_to_encoding(binary()) -> atom() | integer().
binary_to_encoding(Binary) ->
    try bsondoc:binary_to_atom(Binary)
    catch
        _:_ ->
            binary_to_integer(Binary)
    end.

-spec get_uuid_to_customer_dict([uuid()]) -> dict().
get_uuid_to_customer_dict(Uuids) ->
    get_id_to_term_dict(Uuids, fun k_storage_customers:get_customer_by_uuid/1).

-spec get_id_to_network_dict([uuid()]) -> dict().
get_id_to_network_dict(Ids) ->
    get_id_to_term_dict(Ids, fun k_storage_networks:get_network/1).

-spec get_id_to_gateway_dict([uuid()]) -> dict().
get_id_to_gateway_dict(Ids) ->
    get_id_to_term_dict(Ids, fun k_storage_gateways:get_gateway/1).

-spec get_id_to_provider_dict([uuid()]) -> dict().
get_id_to_provider_dict(Ids) ->
    get_id_to_term_dict(Ids, fun k_storage_providers:get_provider/1).

%% ===================================================================
%% Internal
%% ===================================================================

get_id_to_term_dict(Ids, Fun1) when is_function(Fun1, 1) ->
    get_id_to_term_dict(Ids, Fun1, dict:new()).

get_id_to_term_dict([], _Fun1, Dict) ->
    Dict;
get_id_to_term_dict([Id | Ids], Fun1, Dict) ->
    case dict:is_key(Id, Dict) of
        true ->
            get_id_to_term_dict(Ids, Fun1, Dict);
        false ->
            case Fun1(Id) of
                {ok, Term} ->
                    Dict2 = dict:store(Id, Term, Dict),
                    get_id_to_term_dict(Ids, Fun1, Dict2);
                {error, no_entry} ->
                    get_id_to_term_dict(Ids, Fun1, Dict)
            end
    end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

objectid_to_binary_test() ->
    ObjId = {<<83,149,217,23,77,52,173,92,254,208,16,218>>},
    Exp = <<"5395d9174d34ad5cfed010da">>,
    Act = objectid_to_binary(ObjId),
    ?assertEqual(Exp, Act).

binary_to_objectid_test() ->
    Bin = <<"5395d9174d34ad5cfed010da">>,
    Exp = {<<83,149,217,23,77,52,173,92,254,208,16,218>>},
    Act = binary_to_objectid(Bin),
    ?assertEqual(Exp, Act).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
