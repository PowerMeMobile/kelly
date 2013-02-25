-module(k_storage_utils).

-export([
	addr_to_doc/1,
	doc_to_addr/1,

	doc_to_mt_msg_info/1,
	doc_to_mo_msg_info/1
]).

-include_lib("alley_dto/include/addr.hrl").
-include_lib("k_common/include/msg_id.hrl").
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
	SrcAddrDoc = bson:at(sa, Doc),
	DstAddrDoc = bson:at(da, Doc),
	#msg_info{
		client_type = bson:at(ct, Doc),
		customer_id = bson:at(ci, Doc),
		in_msg_id = bson:at(imi, Doc),
		gateway_id = bson:at(gi, Doc),
		out_msg_id = bson:at(omi, Doc),
		type = bson:at(t, Doc),
		encoding = bson:at(e, Doc),
		body = bson:at(b, Doc),
		src_addr = doc_to_addr(SrcAddrDoc),
		dst_addr = doc_to_addr(DstAddrDoc),
		reg_dlr = bson:at(rd, Doc),
		req_time = bson:at(rqt, Doc),
		resp_time = bson:at(rpt, Doc),
		resp_status = bson:at(rps, Doc),
		dlr_time = bson:at(dt, Doc),
		dlr_status = bson:at(ds, Doc)
	}.

-spec doc_to_mo_msg_info(bson:document()) -> #msg_info{}.
doc_to_mo_msg_info(Doc) ->
	SrcAddrDoc = bson:at(sa, Doc),
	DstAddrDoc = bson:at(da, Doc),
	#msg_info{
		customer_id = bson:at(ci, Doc),
		in_msg_id = bson:at(imi, Doc),
		gateway_id = bson:at(gi, Doc),
		type = bson:at(t, Doc),
		encoding = bson:at(e, Doc),
		body = bson:at(b, Doc),
		src_addr = doc_to_addr(SrcAddrDoc),
		dst_addr = doc_to_addr(DstAddrDoc),
		reg_dlr = bson:at(rd, Doc),
		req_time = bson:at(rqt, Doc)
	}.
