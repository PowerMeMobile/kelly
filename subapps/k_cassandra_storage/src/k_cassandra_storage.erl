-module(k_cassandra_storage).

-behaviour(k_gen_storage).

%% k_gen_storage callbacks
-export([
	start_link/0,
	open/2,
	close/1,
	read/1,
	read/2,
	write/3,
	delete/2
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("cassandra_thrift/include/cassandra_types.hrl").

-record(state, {
	connection :: term(),
	keyspace :: string()
}).

%%
%% http://wiki.apache.org/cassandra/API
%%

%% ===================================================================
%% k_gen_storage callbacks
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open(Table::string(), Opts::[tuple()]) -> {ok, string()} | {error, Reason::term()}.
open(Table, Opts) ->
	gen_server:call(?MODULE, {open, Table, Opts}, infinity).

-spec close(Table::string()) -> ok | {error, Reason::term()}.
close(_Table) ->
	ok.

-spec read(Table::string()) -> {ok, Value::term()} | {error, Reason::term()}.
read(Table) ->
	gen_server:call(?MODULE, {read, Table}, infinity).

-spec read(Table::string(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read(Table, Key) ->
	gen_server:call(?MODULE, {read, Table, Key}, infinity).

-spec write(Table::string(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(Table, Key, Value) ->
	gen_server:call(?MODULE, {write, Table, Key, Value}, infinity).

-spec delete(Table::string(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete(Table, Key) ->
	gen_server:call(?MODULE, {delete, Table, Key}, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	Args = application:get_all_env(?APP),
	?log_debug("connecting...", []),
	{ok, {Conn, Keyspace}} = init_connection(Args),
	?log_debug("connected.", []),
	{ok, #state{connection = Conn, keyspace = Keyspace}}.

handle_call({open, Table, _Opts}, _From, State = #state{
	connection = Conn,
	keyspace = Keyspace
}) ->
	try
		CfDef = #cfDef{keyspace = Keyspace, name = Table},
		{Conn1, {ok, _}} = thrift_client:call(Conn, 'system_add_column_family', [CfDef]),
		{reply, {ok, Table}, State#state{connection = Conn1}}
	catch
		throw:{Conn2, {exception, {invalidRequestException, Reason}}} ->
			?log_debug("Caught exception: ~p", [Reason]),
			{reply, {ok, Table}, State#state{connection = Conn2}}
	end;

handle_call({read, Table}, _From, State = #state{
	connection = Conn
}) ->
	try thrift_client:call(Conn,
		'get_range_slices', [
			#columnParent{column_family = Table},
			#slicePredicate{column_names = ["v"], slice_range = undefined},
			#keyRange{start_key = "", end_key = ""},
			?cassandra_ConsistencyLevel_ONE
		]) of
			{Conn1, {ok, KeySlices}} ->
				KeyValuePairs = lists:map(
					fun(#keySlice{
							key = Key,
							columns = [#columnOrSuperColumn{column = #column{value = Value}} | _]
						}) ->
						{decode_key(Key), decode_value(Value)}
					end,
					KeySlices),
				{reply, {ok, KeyValuePairs}, State#state{connection = Conn1}}
	catch
		throw:{Conn2, {exception, #notFoundException{}}} ->
			{reply, {error, no_entry}, State#state{connection = Conn2}}
	end;

handle_call({read, Table, Key}, _From, State = #state{
	connection = Conn
}) ->
	try thrift_client:call(Conn,
		'get', [
			encode_key(Key),
			#columnPath{column_family = Table, column = "v"},
			?cassandra_ConsistencyLevel_ONE
		]) of
			{Conn1, {ok, #columnOrSuperColumn{column = #column{value = Value}}}} ->
				{reply, {ok, decode_value(Value)}, State#state{connection = Conn1}}
	catch
		throw:{Conn2, {exception, #notFoundException{}}} ->
			{reply, {error, no_entry}, State#state{connection = Conn2}}
	end;

handle_call({write, Table, Key, Value}, _From, State = #state{
	connection = Conn
}) ->
	{Conn1, {ok, ok}} = thrift_client:call(Conn,
		'insert', [
			encode_key(Key),
			#columnParent{column_family = Table},
			#column{name="v", value = encode_value(Value), timestamp = get_timestamp()},
			?cassandra_ConsistencyLevel_ONE
		]),
	{reply, ok, State#state{connection = Conn1}};

handle_call({delete, Table, Key}, _From, State = #state{
	connection = Conn
}) ->
	{Conn1, {ok, ok}} = thrift_client:call(Conn,
		'remove', [
			encode_key(Key),
			#columnPath{column_family = Table}, % , column = "v"
			get_timestamp(),
			?cassandra_ConsistencyLevel_ONE
		]),
	{reply, ok, State#state{connection = Conn1}};

handle_call(Request, _From, State = #state{}) ->
	cassandra_thrift:a(),
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec init_connection(Args::[tuple()]) -> {ok, {Conn::term(), Keyspace::string()}} | {error, Reason::term()}.
init_connection(Args) ->
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	Keyspace = proplists:get_value(keyspace, Args),
	{ok, Conn} = thrift_client_util:new(Host, Port, cassandra_thrift, [{framed, true}]),
	%% try to use the specified keyspace.
	try thrift_client:call(Conn, 'set_keyspace', [Keyspace]) of
		{Conn1, {ok, ok}} ->
			{ok, {Conn1, Keyspace}}
	catch
		%% the keyspace isn't available.
		throw:{Conn2, {exception, {invalidRequestException, Reason}}} ->
			?log_debug("Caught exception: ~p. Create keyspace: ~s", [Reason, Keyspace]),
			Opts = dict:from_list([{"replication_factor", "1"}]),
			KsDef = #ksDef{name = Keyspace,
						  strategy_class = "SimpleStrategy",
						  strategy_options = Opts},
			%% add the keyspace.
			{Conn3, {ok, _}} = thrift_client:call(Conn2, 'system_add_keyspace', [KsDef]),
			%% use the keyspace.
			{Conn4, {ok, ok}} = thrift_client:call(Conn3, 'set_keyspace', [Keyspace]),
			{ok, {Conn4, Keyspace}}
	end.

encode_key(Key) ->
	term_to_binary(Key).

encode_value(Value) ->
	term_to_binary(Value).

decode_key(Key) ->
	binary_to_term(Key).

decode_value(Value) ->
	binary_to_term(Value).

get_timestamp() ->
	{Mega, Sec, Micro} = erlang:now(),
	(Mega * 1000000 + Sec) * 1000000 + Micro.
