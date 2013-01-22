-module(k_storage_utils).

-export([
	addr_to_doc/1,
	doc_to_addr/1
]).

-include_lib("alley_dto/include/addr.hrl").

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
