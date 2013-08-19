-module(k_snmp).

-behaviour(gen_fsm).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_storage/include/gateway.hrl").
-include_lib("alley_common/include/gen_fsm_spec.hrl").
-include("snmp_task.hrl").

-define(USER, "user").
-define(TARGET, "just").

-define(destroy, 6).
-define(createAndWait, 5).
-define(createAndGo, 4).
-define(notReady, 3).
-define(notInService, 2).
-define(active, 1).

%% API
-export([
	start_link/0,
	get_column_val/2,
	set_row/3,
	del_row/2,

	set_customer/3,
	delete_customer/1,

	set_gateway/3,
	delete_gateway/1,

	set_connection/2,
	delete_connection/2,

	set_setting/2,
	delete_setting/2
]).

%% Callbacks
-export([
	init/1,

	ready/2,
	ready/3,
	sleep/2,
	sleep/3,

	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4
]).

-record(state, {
	time = 10000 :: integer()
}).

-record(varbind, {
	oid 	:: list(),
	type 	:: atom(),
	value 	:: term(),
	int 	:: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	{ok, Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
	process_queue(),
	{ok, Pid}.

-spec get_column_val(snmp_column_name(), snmp_index()) -> snmp_result().
get_column_val(ColumnName, Index) ->
	{ok, [OID]} = snmpm:name_to_oid(ColumnName),
	Result = snmpm:sync_get(?USER, ?TARGET, [OID ++ Index]),
	parse_snmp_result(Result).

-spec set_row(snmp_column_name(), snmp_index(), snmp_value_list()) -> ok.
set_row(TableName, Index, ValueList) ->
	Task = #task{function = fun set_row/1, args = {TableName, Index, ValueList}},
	process_task(Task).

-spec del_row(snmp_column_name(), snmp_index()) -> ok.
del_row(TableName, Index)->
	Task = #task{function = fun del_row/1, args = {TableName, Index}},
	process_task(Task).

-spec set_customer(binary(), integer(), integer()) -> ok.
set_customer(ID, RPS, Priority) when
					is_binary(ID) andalso
					is_integer(RPS) andalso
					is_integer(Priority) ->
	set_row(cst, binary_to_list(ID), [
		{cstRPS, RPS},
		{cstPriority, Priority}]).

-spec delete_customer(binary()) -> ok.
delete_customer(ID) when is_binary(ID) ->
	del_row(cst, binary_to_list(ID)).

-spec set_gateway(binary(), binary(), integer()) -> ok.
set_gateway(ID, Name, RPS) when
					is_binary(ID) andalso
					is_binary(Name) andalso
					is_integer(RPS) ->
	set_row(gtw, binary_to_list(ID),
			[{gtwName, binary_to_list(Name)}, {gtwRPS, RPS}]).

-spec delete_gateway(binary()) -> ok.
delete_gateway(ID) when is_binary(ID) ->
	del_row(gtw, binary_to_list(ID)).

-spec set_connection(binary(), #connection{}) -> ok.
set_connection(GtwID, Conn = #connection{}) when is_binary(GtwID) ->
	set_row(cnn, binary_to_list(GtwID) ++ [Conn#connection.id], [
		{cnnAddr, convert_to_snmp_ip(Conn#connection.host)},
		{cnnPort, Conn#connection.port},
		{cnnType, bind_type_to_integer(Conn#connection.bind_type)},
		{cnnSystemId, binary_to_list(Conn#connection.system_id)},
		{cnnPassword, binary_to_list(Conn#connection.password)},
		{cnnSystemType, binary_to_list(Conn#connection.system_type)},
		{cnnAddrTon, Conn#connection.addr_ton},
		{cnnAddrNpi, Conn#connection.addr_npi},
		{cnnAddrRange, binary_to_list(Conn#connection.addr_range)}
	]).

-spec delete_connection(binary(), integer()) -> ok.
delete_connection(GtwID, ConnID) when
					is_binary(GtwID) andalso
					is_integer(ConnID) ->
	del_row(cnn, binary_to_list(GtwID) ++ [ConnID]).

-spec set_setting(binary(), #setting{}) -> ok.
set_setting(GtwID, Setting = #setting{}) when is_binary(GtwID) ->
	Index = binary_to_list(GtwID) ++ [size(Setting#setting.name)] ++ binary_to_list(Setting#setting.name),
	set_row(sts, Index,
				[{stsValue, binary_to_list(Setting#setting.value)}]).

-spec delete_setting(binary(), integer()) -> ok.
delete_setting(GtwID, SettingID) when
					is_binary(GtwID) andalso
					is_binary(SettingID) ->
	Index = binary_to_list(GtwID) ++ [size(SettingID)] ++ binary_to_list(SettingID),
	del_row(sts, Index).

%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

init(_Args) ->
	{ok, ready, #state{}}.

-spec sleep(process_queue, #state{}) -> {next_state, sleep, #state{}};
			(wake_up, #state{}) -> {next_state, ready, #state{}};
			(term(), #state{}) -> {stop, {bad_event, term()}, #state{}}.
sleep(process_queue, State = #state{}) ->
	{next_state, sleep, State};
sleep(wake_up, State = #state{}) ->
	process_queue(),
	{next_state, ready, State};
sleep(Event, State = #state{}) ->
	{stop, {bad_event, Event}, State}.

-spec ready(process_queue, #state{}) ->
	{next_state, ready, #state{}} |
	{next_state, sleep, #state{}};
			(term(), #state{}) ->
	{stop, {bad_event, term()}, #state{}}.
ready(process_queue, State = #state{time = Time}) ->
	case next() of
	{ok, #task{id = Id, function = Fun, args = Args}} ->
		case Fun(Args) of
			{ok, ok} ->
				ack(Id),
				process_queue(),
				{next_state, ready, State};
			_Error ->
				start_timer(Time),
				{next_state, sleep, State}
		end;
	{ok, []} ->
		{next_state, ready, State}
	end;
ready(Event, State = #state{}) ->
	{stop, {bad_event, Event}, State}.

-spec sleep(term(), term(), term()) ->
	{stop, {bad_sync_event, term()}, term()}.
sleep(Event, _From, State) ->
	{stop, {bad_sync_event, Event}, State}.

-spec ready(term(), term(), term()) ->
	{stop, {bad_sync_event, term()}, term()}.
ready(Event, _From, State) ->
	{stop, {bad_sync_event, Event}, State}.

handle_event(Event, _StateName, State = #state{}) ->
	{stop, {bad_event, Event}, State}.

handle_sync_event(Event, _From, _StateName, State = #state{}) ->
	{stop, {bad_sync_event, Event}, State}.

handle_info(_Info, StateName, State = #state{}) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State = #state{}) ->
	ok.

code_change(_OldVsn, StateName, State = #state{}, _Extra) ->
	{ok, StateName, State}.

%% ===================================================================
%% Internals
%% ===================================================================

%%% Queue functions

next() ->
	k_snmp_tasks_queue:next().

save_task(Task) ->
	k_snmp_tasks_queue:save(Task).

ack(Id) ->
	k_snmp_tasks_queue:ack(Id).
%%%

start_timer(Time) ->
	{ok, _TRef} = timer:apply_after(Time, gen_fsm, send_event, [?MODULE, wake_up]).

process_queue() ->
	gen_fsm:send_event(?MODULE, process_queue).

process_task(Task) ->
	save_task(Task),
	process_queue().

set_row({TableName, Index, ValueList}) ->
	case is_exist(TableName, Index) of
		exist -> update(Index, ValueList);
		notExist -> create(TableName, Index, ValueList);
		incorrectState -> recreate(TableName, Index, ValueList);
		Any -> Any
	end.

del_row({TableName, Index}) ->
	case TableName of
		gtw ->
			set(Index, [{gtwStatus, ?destroy}]);
		sts ->
			set(Index, [{stsStatus, ?destroy}]);
		cst ->
			set(Index, [{cstStatus, ?destroy}]);
		cnn ->
			set(Index, [{cnnStatus, ?destroy}]);
		_Any ->
			{error, noSuchTable}
	end.


recreate(TableName, Index, ValueList) ->
	del_row({TableName, Index}),
	create(TableName, Index, ValueList).

create(TableName, Index, ValueList) ->
	case TableName of
		gtw ->
			update(Index, [{gtwStatus, ?createAndWait}] ++ ValueList ++ [{gtwStatus, ?active}]);
		sts ->
			update(Index, [{stsStatus, ?createAndWait}] ++ ValueList ++ [{stsStatus, ?active}]);
		cst ->
			update(Index, [{cstStatus, ?createAndWait}] ++ ValueList ++ [{cstStatus, ?active}]);
		cnn ->
			update(Index, [{cnnStatus, ?createAndWait}] ++ ValueList ++ [{cnnStatus, ?active}]);
		_Any ->
			{error, noSuchTable}
	end.

update(Index, ValueList) ->
	set(Index, ValueList).


set(Index, ValueList) ->
	set(Index, ValueList, _InitResult = {ok, init}).

set(_Index, _ValueList = [], _LastResult = {ok, _}) ->
	{ok, ok};
set(Index, [{ColumnName, Value} | RestValueList], _LastResult = {ok, _}) ->
	{ok, [OID]} = snmpm:name_to_oid(ColumnName),
	Result = snmpm:sync_set(?USER, ?TARGET, [{OID ++ Index, Value}]),
	set(Index, RestValueList, parse_snmp_result(Result));
set(_Index, _ValueList, ErrorResult) ->
	{error, ErrorResult}.

is_exist(TableName, Index)->
	{ok, ColumnName} = status_column_name(TableName),
	GetResult = get_column_val(ColumnName, Index),
	case GetResult of
		{ok, ?active} -> exist;
		{ok, Some} when is_integer(Some) -> incorrectState;
		{error, {timeout, T}} -> {error, {timeout, T}};
		{_Error, _More} -> notExist
	end.

status_column_name(TableName) ->
	case TableName of
		gtw -> {ok, gtwStatus};
		sts -> {ok, stsStatus};
		cst -> {ok, cstStatus};
		cnn -> {ok, cnnStatus};
		_Any -> {error, noSuchTable}
	end.

parse_snmp_result(Result) ->
	case Result of
		{ok, { noError, 0, [#varbind{value = Value}]}, _Remaining} ->
			case Value of
				noSuchObject ->
					{error, noSuchObject};
				noSuchInstance ->
					{error, noSuchInstance};
				AnyValue->
					{ok, AnyValue}
			end;
		{ok, {ErState, _ErInd, _Varbind}, _Remaining} ->
			{error, ErState};
		{error, Reason} ->
			{error, Reason}
	end.

%% convert "127.0.0.1" to [127,0,0,1]
convert_to_snmp_ip(Addr) when is_binary(Addr) ->
	Tokens = string:tokens(binary_to_list(Addr), "."),
	[list_to_integer(Token) || Token <- Tokens].

bind_type_to_integer(transmitter) -> 1;
bind_type_to_integer(receiver)    -> 2;
bind_type_to_integer(transceiver) -> 3.
