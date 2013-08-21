-module(k_worker_sup).

-behaviour(supervisor).

-export([
	start_link/0
]).
-export([
	process/2
]).

-export([
	init/1
]).

-include_lib("alley_common/include/supervisor_spec.hrl").
-include("amqp_req.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec process(atom(), #amqp_req{}) -> {ok, {pid(), reference()}}.
process(Module, Req) ->
	{SupPid, _Value} = gproc:await({n, l, ?MODULE}),
	{ok, WPid} = supervisor:start_child(SupPid, []),
    MonRef = erlang:monitor(process, WPid),
	gen_server:cast(WPid, {process, {Module, Req, self()}}),
	{ok, {WPid, MonRef}}.

init(_Args) ->
	gproc:add_local_name(?MODULE),
	{ok, {{simple_one_for_one, 0, 1},
			[{k_worker, {k_worker, start_link, []}, temporary, brutal_kill, worker, [k_worker]}]}}.
