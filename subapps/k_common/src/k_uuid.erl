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
	to_binary/1,
	is_valid/1
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

%% @doc Predicate for checking that supplied UUID is valid.
-spec is_valid(Uuid::uuid() | uuid_string()) -> true | false.
%% XXX special nil UUID is valid
is_valid(<<0:128>>) -> true;
is_valid(Uuid = <<_:128>>) ->
    is_valid(variant(Uuid), Uuid);
is_valid(UuidStr) when is_list(UuidStr) ->
    is_valid(to_binary(UuidStr));
is_valid(_) ->
    erlang:error(badarg).

%% @private
%% @doc Predicate for checking that supplied UUID is valid, takes variant as
%%      argument and returns validity depending on UUID version.
-spec is_valid(Variant::atom(), Uuid::uuid()) -> true | false.
is_valid(rfc4122, Uuid) ->
    Version = version(Uuid),
    case Version of
        1 -> true;
        3 -> true;
        4 -> true;
        5 -> true;
        _ -> false
    end;
is_valid(_, _) -> false.

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

%% @doc Return variant for supplied UUID.
-spec variant(Uuid::uuid() | uuid_string()) -> reserved_microsoft
                                             | reserved_ncs
                                             | resered_future
                                             | rfc4122.
variant(<<_:128>> = Uuid) ->
    <<_:64, V2:1, V1:1, V0:1, _:61>> = Uuid,
    case {V2, V1, V0} of
        {0, _, _} -> reserved_ncs;
        {1, 0, _} -> rfc4122;
        {1, 1, 0} -> reserved_microsoft;
        {1, 1, 1} -> reserved_future
    end;
variant(UuidStr) when is_list(UuidStr) ->
    variant(uuid:to_binary(UuidStr));
variant(_) ->
    erlang:error(badarg).


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
