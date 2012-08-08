-module(k_storage_parts_mgr).

-behaviour(gen_server).

%% API
-export([
	start_link/1
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
-include("daily_cfg.hrl").
-include("manifest.hrl").

-define(TIMER_INTERVAL, 10000).

-type handle() :: kv_storage:handle().

-record(state, {
	storage_name :: atom(),
	manifest :: #manifest{},
	tick_ref :: reference(),
	parts :: [handle()],
	parts_to_close = [] :: [handle()]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(string()) -> {ok, pid()}.
start_link(StorageName) ->
	gen_server:start_link({local, StorageName}, ?MODULE, [StorageName], []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([StorageName]) ->
	?log_debug("init(~p)", [StorageName]),

	{ok, Parts, Manifest} = open_parts(StorageName),

	TickRef = make_ref(),
	setup_alarm(TickRef),

	?log_debug("started(~p)", [StorageName]),
	{ok, #state{
		storage_name = StorageName,
		manifest = Manifest,
		tick_ref = TickRef,
		parts = Parts
	}}.

handle_call(perform_rotation, _From, State = #state{}) ->
	{ok, NewState} = perform_rotation(State),
	{reply, ok, NewState};

handle_call({set, Key, Value}, _From, State = #state{
	parts = [Part|_]
}) ->
	Res = kv_storage:write(Part, Key, Value),
	{reply, Res, State};

handle_call({get, Key}, _From, State = #state{
	parts = Parts
}) ->
	Res = find_by_id(Key, Parts),
	{reply, Res, State};

handle_call({delete, Key}, _From, State = #state{
	parts = Parts
}) ->
	Res = delete_by_id(Key, Parts),
	{reply, Res, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({tick, TickRef}, State = #state{
	tick_ref = TickRef
}) ->
	{ok, NewState} = on_tick(State),
	setup_alarm(TickRef),
	{noreply, NewState};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

setup_alarm(TickRef) ->
	timer:send_after(?TIMER_INTERVAL, self(), {tick, TickRef}).

on_tick(State = #state{manifest = Manifest}) ->
	LastRotationTime = Manifest#manifest.last_rotated,
	case should_rotate(LastRotationTime) of
		true ->
			{ok, NewState} = perform_rotation(State),
			{ok, _NewState2} = close_old_partitions(NewState);
		_ ->
			% ?log_debug("should not rotate(~p)", [DbName]),
			{ok, State}
	end.

should_rotate(LastRotationTime) ->
	LastRotationTime /= current_rotation_time().

current_rotation_time() ->
	erlang:date().

perform_rotation(State = #state{
	storage_name = StorageName
}) ->
	?log_debug("should rotate(~p)", [StorageName]),
	{ok, NewState, _NewParts} = add_partition(State),
	?log_debug("rotation[add new part] complete(~p)", [StorageName]),
	{ok, NewState}.

close_old_partitions(State = #state{
	storage_name = StorageName,
	parts_to_close = PartsToClose
}) ->
	?log_debug("closing the old partitions(~p)...", [StorageName]),
	lists:map(
		fun(Part) ->
			?log_debug("closing old partition(~p) ~p", [StorageName, Part]),
			ok = kv_storage:close(Part)
		end,
		PartsToClose),
	?log_debug("rotation[closing old parts] complete(~p)", [StorageName]),
	{ok, State #state{parts_to_close = []}}.

add_partition_to_manifest(StorageName, Manifest = #manifest{
	last_rotated = LastRotated,
	partitions = PartitionsOld
}) ->
	[Today|_] = PartitionsOld,
	TodaySeq = Today#daily_cfg.seq,
	TomorrowSeq = TodaySeq + 1,

	Tomorrow = #daily_cfg{
		seq = TomorrowSeq,
		name = build_name(TomorrowSeq, StorageName)
	},

	CurrentRotationTime = current_rotation_time(),
	?log_debug("Rotating(~p) [~p/~p  ->  ~p/~p]",
		[StorageName, LastRotated, TodaySeq, CurrentRotationTime, TomorrowSeq]),

	Partitions = [Tomorrow|PartitionsOld],
	ActualPartsCount = length(Partitions),
	DailyPartsCount = daily_parts_count(),

	PartitionsLeft =
		case ActualPartsCount > DailyPartsCount of
			true ->
				lists:sublist(Partitions, DailyPartsCount);
			false ->
				Partitions
		end,

	{ok, Manifest#manifest{
			last_rotated = CurrentRotationTime,
			partitions = PartitionsLeft
		}, Tomorrow}.

-spec add_tomorrow_partition(atom(), [handle()], string()) -> {ok, [handle()], [handle()]}.
add_tomorrow_partition(StorageName, Parts, TomorrowPartName) ->
	{ok, NewPart} = kv_storage:open(TomorrowPartName),
	?log_debug("Opened tomorrow partition: (~p)[~p]", [StorageName, NewPart]),

	NewParts = [NewPart|Parts],

	%% Purge the older partitions
	ActualPartsCount = length(NewParts),
	DailyPartsCount = daily_parts_count(),
	{PartsLeft, PartsToClose} =
		case (ActualPartsCount) > DailyPartsCount of
			true ->
				{
					lists:sublist(NewParts, DailyPartsCount),
					lists:sublist(lists:reverse(NewParts), ActualPartsCount - DailyPartsCount)
				};
			false ->
				{NewParts, []}
		end,

	{ok, PartsLeft, PartsToClose}.

-spec add_partition(#state{}) -> {ok, #state{}, handle()}.
add_partition(State = #state{
	storage_name = StorageName,
	manifest = Manifest,
	parts = Parts
}) ->
	{ok, NewManifest, Tomorrow} = add_partition_to_manifest(StorageName, Manifest),
	ok = write_manifest(StorageName, NewManifest),
	?log_debug("New manifest written (~p)[~p]", [StorageName, NewManifest#manifest.last_rotated]),

	TomorrowPartName = Tomorrow#daily_cfg.name,
	{ok, NewParts, PartsToClose} = add_tomorrow_partition(StorageName, Parts, TomorrowPartName),
	{ok, State#state{
		manifest = NewManifest,
		parts = NewParts,
		parts_to_close = PartsToClose
	}, NewParts}.

-spec open_parts(atom()) -> {ok, [handle()], #manifest{}} | {error, Reason::term()}.
open_parts(StorageName) ->
	case read_manifest(StorageName) of
		{ok, Manifest = #manifest{partitions = Partitions}} ->
			OpenParts =
				lists:map(
					fun(#daily_cfg{name = Name}) ->
						{ok, Part} = kv_storage:open(Name),
						?log_debug("Opened partition: (~p)[~p]", [StorageName, Part]),
						Part
					end,
					Partitions),
			{ok, OpenParts, Manifest};
		{error, enoent} ->
			%% manifest doesn't exist yet.
			Seq = 1,
			Name = build_name(Seq, StorageName),
			{ok, Part} = kv_storage:open(Name),
			?log_debug("Created partition: (~p)[~p]", [StorageName, Part]),

			Manifest = #manifest{
				last_rotated = current_rotation_time(),
				partitions = [#daily_cfg{seq = Seq, name = Name}]
			},

			ok = write_manifest(StorageName, Manifest),
			?log_debug("New manifest written (~p)[~p]", [StorageName, Manifest#manifest.last_rotated]),
			{ok, [Part], Manifest};
		Error ->
			Error
	end.

-spec read_manifest(atom()) -> {ok, #manifest{}} | {error, Reason::term()}.
read_manifest(StorageName) ->
	ManifestPath = manifest_file_path(StorageName),
	case file:consult(ManifestPath) of
		{ok, [Manifest]} ->
			{ok, Manifest};
		Error ->
			Error
	end.

-spec write_manifest(atom(), #manifest{}) -> ok | {error, Reason::term()}.
write_manifest(StorageName, NewManifest) ->
	NewManifestSerialized = io_lib:format("~p.", [NewManifest]),
	ManifestFilePath = manifest_file_path(StorageName),
	file:write_file(ManifestFilePath, NewManifestSerialized).

-spec build_name(integer(), atom()) -> string().
build_name(Seq, StorageName) when is_integer(Seq)
							 andalso is_atom(StorageName) ->
	{ok, NameFmt} = application:get_env(?APP, name_fmt),
	lists:flatten(io_lib:format(NameFmt, [Seq, StorageName])).

-spec daily_parts_count() -> integer().
daily_parts_count() ->
	{ok, PC} = application:get_env(?APP, daily_parts_count),
	PC.

-spec manifest_file_path(atom()) -> string().
manifest_file_path(StorageName) when is_atom(StorageName) ->
	{ok, ManifestFmt}  = application:get_env(?APP, manifest_fmt),
	lists:flatten(io_lib:format(ManifestFmt, [StorageName])).

-spec find_by_id(ID::term(), Partitions::[handle()]) -> {ok, any()} | {error, any()}.
find_by_id(_ID, []) ->
	{error, no_entry};
find_by_id(ID, [Part|SoFar]) ->
	case kv_storage:read(Part, ID) of
		{ok, Entry} ->
			{ok, Entry};
		{error, no_entry} ->
			find_by_id(ID, SoFar);
		Other ->
			{error, Other}
	end.

-spec delete_by_id(ID::term(), Partitions::[handle()]) -> ok | {error, any()}.
delete_by_id(_ID, []) ->
	{error, no_entry};
delete_by_id(ID, [Part|SoFar]) ->
	case kv_storage:delete(Part, ID) of
		ok ->
			ok;
		{error, no_entry} ->
			delete_by_id(ID, SoFar);
		Other ->
			{error, Other}
	end.
