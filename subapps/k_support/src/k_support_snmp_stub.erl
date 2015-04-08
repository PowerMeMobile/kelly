-module(k_support_snmp_stub).

-export([
    stub/0
]).

%% ===================================================================
%% API
%% ===================================================================

-spec stub() -> ok.
stub() ->
    ok = application:start(meck),
    ok = meck:new(k_support_snmp, [non_strict]),
    ok = meck:expect(k_support_snmp, name_to_oid, fun(_) -> {ok, []} end),
    ok = meck:expect(k_support_snmp, sync_get, fun(_,_) -> {ok, 1} end),
    ok = meck:expect(k_support_snmp, sync_set, fun(_,_) -> ok end),
    ok.
