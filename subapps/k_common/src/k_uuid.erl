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

-export([
	newid/0,
	to_string/1,
	is_v4/1,
	to_binary/1
]).

-record(state, {}).

-type uuid()::binary().
-type uuid_string()::string().

-include("gen_server_spec.hrl").

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

%% @doc Initialize the module.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Generate new UUID.
-spec newid() -> uuid().
newid() ->
	gen_server:call(?MODULE, newid, infinity).

%% @doc Convert UUID to a string.
-spec to_string(uuid()) -> uuid_string().
to_string(U) ->
	lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

%% @doc Check for true uuid format
-spec is_v4(Uuid::uuid() | uuid_string()) -> true | false.
is_v4(Uuid) -> 4 =:= version(Uuid).

%% @private
-spec version(Uuid::uuid() | uuid_string()) -> integer().
version(<<_:128>> = Uuid) ->
    <<_:48, Version:4, _:76>> = Uuid,
    Version;
version(UuidStr) when is_list(UuidStr) ->
    version(to_binary(UuidStr));
version(_) ->
    erlang:error(badarg).

%% @doc Format UUID binary from string.
-spec to_binary(UuidStr::uuid_string()) -> uuid().
to_binary(UuidStr) when is_list(UuidStr) ->
    case length(UuidStr) of
        36 -> to_binary(pretty, UuidStr);
        32 -> to_binary(simple, UuidStr);
        _  -> erlang:error(badarg)
    end;
to_binary(_) ->
    erlang:error(badarg).

%% @private
-spec to_binary(simple | pretty, UuidStr::uuid_string()) -> uuid().
to_binary(simple, UuidStr) ->
    Num = hex_to_int(UuidStr),
    <<Num:128>>;
to_binary(pretty, UuidStr) ->
    Parts = string:tokens(UuidStr, "$-"),
    [I0, I1, I2, I3, I4] = [hex_to_int(Part) || Part <- Parts],
    <<I0:32, I1:16, I2:16, I3:16, I4:48>>.

%% ===================================================================
%% Gen Server Callback Functions Definitions
%% ===================================================================

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

%% ===================================================================
%% Internal Functions Definitions
%% ===================================================================

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

%% @private
%% @doc  Convert from hexadecimal digit represented as string to decimal.
-spec hex_to_int(Hex::string()) -> integer().
hex_to_int(Hex) ->
    {ok, [D], []} = io_lib:fread("~16u", Hex),
    D.

