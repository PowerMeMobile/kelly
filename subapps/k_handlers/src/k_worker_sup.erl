-module(k_worker_sup).

-behaviour(supervisor).

-export([
	start_link/0
]).
-export([
	process/4
]).

-export([
	init/1
]).

-include_lib("k_common/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec process(atom(), binary(), binary(), pid()) -> {ok, pid()}.
process(Module, ContentType, Message, Channel) ->
	{SupPid, _Value} = gproc:await({n, l, ?MODULE}),
	{ok, Pid} = supervisor:start_child(SupPid, []),
	gen_server:cast(Pid, {process, {Module, ContentType, Message, Channel}}),
	{ok, Pid}.

init(_Args) ->
	gproc:add_local_name(?MODULE),
	{ok, {{simple_one_for_one, 0, 1},
			[{k_worker, {k_worker, start_link, []}, temporary, brutal_kill, worker, [k_worker]}]}}.
