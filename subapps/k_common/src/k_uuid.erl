-module(k_uuid).

-behaviour(gen_server).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-export([start_link/0]).

-export([newid/0, to_string/1]).

-record(state, {}).

%% @doc Initialize the module.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
	random:seed(erlang:time()),
	{ok, #state{}}.

%% @private
handle_call(newid, _From, State) ->
	{reply, v4(), State};
handle_call(Request, From, State) ->
	{stop, {bad_arg, call, Request, From, State}, bad_arg, State}.

%% @private
handle_cast(Msg, State) ->
	{stop, {bad_arg, cast, Msg, State}, State}.

%% @private
handle_info(Info, State) ->
	{stop, {bad_arg, info, Info, State}, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, _State, _Extra) ->
	{stop, not_supported}.

%% API

%% @doc Generate new UUID.
newid() ->
	gen_server:call(?MODULE, newid, infinity).

%% @doc Convert UUID to a string.
to_string(U) ->
	lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).


%% Internal
v4() ->
	v4(
		random:uniform( trunc(math:pow(2, 48)) ) - 1,
		random:uniform( trunc(math:pow(2, 12)) ) - 1,
		random:uniform( trunc(math:pow(2, 32)) ) - 1,
		random:uniform( trunc(math:pow(2, 30)) ) - 1
	).

v4(R1, R2, R3, R4) ->
	<<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
	[TL, TM, THV, CSR, CSL, N].


