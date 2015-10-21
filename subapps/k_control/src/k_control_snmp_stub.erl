-module(k_control_snmp_stub).

-export([
    enable/0,
    disable/0
]).

%% ===================================================================
%% API
%% ===================================================================

-spec enable() -> ok.
enable() ->
    ok = application:start(meck),
    ok = meck:new(k_control_snmp, [non_strict]),
    ok = meck:expect(k_control_snmp, name_to_oid, fun(_) -> {ok, []} end),
    ok = meck:expect(k_control_snmp, sync_get, fun(_,_) -> {ok, 1} end),
    ok = meck:expect(k_control_snmp, sync_set, fun(_,_) -> ok end),
    ok.

-spec disable() -> ok.
disable() ->
    ok = meck:unload(k_control_snmp),
    {module, _} = code:load_file(k_control_snmp),
    ok = application:stop(meck).
