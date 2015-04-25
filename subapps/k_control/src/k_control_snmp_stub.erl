-module(k_control_snmp_stub).

-export([
    stub/0
]).

%% ===================================================================
%% API
%% ===================================================================

-spec stub() -> ok.
stub() ->
    ok = application:start(meck),
    ok = meck:new(k_control_snmp, [non_strict]),
    ok = meck:expect(k_control_snmp, name_to_oid, fun(_) -> {ok, []} end),
    ok = meck:expect(k_control_snmp, sync_get, fun(_,_) -> {ok, 1} end),
    ok = meck:expect(k_control_snmp, sync_set, fun(_,_) -> ok end),
    ok.
