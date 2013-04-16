-module(k_storage_utils).

-export([
	addr_to_doc/1,
	doc_to_addr/1,

	doc_to_mt_msg_info/1,
	doc_to_mo_msg_info/1
]).

-include_lib("alley_dto/include/addr.hrl").
-include_lib("k_common/include/msg_info.hrl").

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

-spec doc_to_mt_msg_info(bson:document()) -> #msg_info{}.
doc_to_mt_msg_info(Doc) ->
	ObjId = bsondoc:at('_id', Doc),
	SrcAddrDoc = bsondoc:at(sa, Doc),
	DstAddrDoc = bsondoc:at(da, Doc),
	#msg_info{
		msg_id = objectid_to_binary(ObjId),
		client_type = bsondoc:binary_to_atom(bsondoc:at(ct, Doc)),
		customer_id = bsondoc:at(ci, Doc),
		user_id = bsondoc:at(ui, Doc),
		in_msg_id = bsondoc:at(imi, Doc),
		gateway_id = bsondoc:at(gi, Doc),
		out_msg_id = bsondoc:at(omi, Doc),
		type = bsondoc:binary_to_atom(bsondoc:at(t, Doc)),
		encoding = bsondoc:binary_to_atom(bsondoc:at(e, Doc)),
		body = bsondoc:at(b, Doc),
		src_addr = doc_to_addr(SrcAddrDoc),
		dst_addr = doc_to_addr(DstAddrDoc),
		status = bsondoc:at(s, Doc),
		reg_dlr = bsondoc:at(rd, Doc),
		req_time = bsondoc:at(rqt, Doc),
		resp_time = bsondoc:at(rpt, Doc),
		dlr_time = bsondoc:at(dt, Doc)
	}.

-spec doc_to_mo_msg_info(bson:document()) -> #msg_info{}.
doc_to_mo_msg_info(Doc) ->
	ObjId = bsondoc:at('_id', Doc),
	SrcAddrDoc = bsondoc:at(sa, Doc),
	DstAddrDoc = bsondoc:at(da, Doc),
	#msg_info{
		msg_id = objectid_to_binary(ObjId),
		customer_id = bsondoc:at(ci, Doc),
		in_msg_id = bsondoc:at(imi, Doc),
		gateway_id = bsondoc:at(gi, Doc),
		type = bsondoc:binary_to_atom(bsondoc:at(t, Doc)),
		encoding = bsondoc:binary_to_atom(bsondoc:at(e, Doc)),
		body = bsondoc:at(b, Doc),
		src_addr = doc_to_addr(SrcAddrDoc),
		dst_addr = doc_to_addr(DstAddrDoc),
		reg_dlr = bsondoc:at(rd, Doc),
		req_time = bsondoc:at(rqt, Doc)
	}.

objectid_to_binary({ObjId}) ->
	list_to_binary(
		lists:flatten(
			[io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(ObjId)]
		)
	).
