-module(test).
-export([benchmark/1, benchmark/2, for/3]).
-export([benchmark_storages/2]).

benchmark(Fun) ->
    statistics(runtime),
    statistics(wall_clock),
	Fun(),
    {_, Runtime} = statistics(runtime),
    {_, Wallclock} = statistics(wall_clock),
	RuntimeSec = Runtime / 1000.0,
    WallclockSec = Wallclock / 1000.0,
    io:format("Erlang Elapsed ~p (runtime) ~p (wall clock) seconds~n", [RuntimeSec, WallclockSec]).

for(I, Max, _) when I > Max ->
    ok;
for(I, Max, F) ->
    F(),
    for(I+1, Max, F).

benchmark(File, Fun) ->
    statistics(wall_clock),
	Fun(),
    {_, Wallclock} = statistics(wall_clock),
    WallclockSec = Wallclock / 1000.0,
    io:format(File, "~p", [WallclockSec]).

benchmark_storages(TableName, RecordsCount) ->
	Storages = [
		{"Kyte", k_kyte_storage, [
			{name_suffix, ".kch"},
			{parts, 2},
			{key_codec, etf},
			{val_codec, etf}
		]},
		{"MongoDB", k_mongodb_storage, []},
		{"Riak", k_riak_storage, []},
		%% {"PostgreSQL", k_postgresql_storage, []},
		%% {"MySQL(MyISAM)", k_mysql_storage, [
		%% 	{engine, "MyISAM"}
		%% ]},
		%% {"MySQL(InnoDB)", k_mysql_storage, [
		%% 	{engine, "InnoDB"}
		%% ]},
		{"Cassandra", k_cassandra_storage, []}
	],
	{ok, F} = file:open(io_lib:format("~s_~p.dat", [TableName, RecordsCount]), [write]),
	lists:foreach(
		fun({{StorageName, StorageEngine, StorageOpts}, Index}) ->
			io:format(F, "~s\t", [StorageName]),
			{ok, T} = StorageEngine:open(lists:flatten(io_lib:format("~s_~p", [TableName, Index])), StorageOpts),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							StorageEngine:write(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N},
							"1c391c64-a038-11e1-b812-00269e42f7a5")
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							StorageEngine:write(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N},
							"1c391c64-a038-11e1-b812-00269e42f7a5")
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							StorageEngine:read(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N})
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							StorageEngine:delete(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N})
						end,
						lists:seq(1, RecordsCount))
				end),
		   	StorageEngine:close(T),
			io:format(F, "~n", [])
		end,
		lists:zip(Storages, lists:seq(1, length(Storages)))),
	file:close(F).
