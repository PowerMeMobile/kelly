-module(k_storage_reports).

-export([
	stats_report_frequency/0,

	msg_stats_report1/1,
	msg_stats_report2/1,
	msg_stats_report/3,

	gtw_stats_report/1,
	gtw_stats_report/2
]).

-spec stats_report_frequency() -> integer().
stats_report_frequency() ->
	60.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Message Stats
%%

-spec msg_stats_report(ReportType::integer(), From::os:timestamp(), To::os:timestamp()) -> {ok, term()} | {error, Reason::any()}.
msg_stats_report(ReportType, From, To) when From < To ->
	{FromFloor, ToCeiling} = align_time_range(From, To),
	Filenames = get_msg_stats_file_list(ReportType, FromFloor, ToCeiling),
	Reports =
		lists:foldr(
		  fun(Filename, SoFar) ->
				  case k_storage_util:read_term_from_file(Filename) of
					  {ok, []} ->
						  SoFar;
					  {ok, Records} ->
						  AnnotatedRecords = annotate_msg_stats_report(ReportType, Records),
						  AnnotatedRecords ++ SoFar;
					  {error, _Reason} ->
						  %?log_debug("Missing msg stats report: ~p", [Filename])
						  SoFar
				  end
		  end, [], Filenames),
	AnnotatedReport = annotate_msg_stats_report_bunch(ReportType, Reports),
	{ok, AnnotatedReport}.

-spec annotate_msg_stats_report_bunch(ReportType::integer(), Records::[tuple()]) -> {term(), Records::[tuple()]}.
annotate_msg_stats_report_bunch(1, Records) ->
	{customers, Records};
annotate_msg_stats_report_bunch(2, Records) ->
	{networks, Records}.

-spec annotate_msg_stats_report(ReportType::integer(), Records::[tuple()]) -> [tuple()].
annotate_msg_stats_report(1, Records) ->
	annotate_msg_stats_report1(Records);
annotate_msg_stats_report(2, Records) ->
	annotate_msg_stats_report2(Records).

-spec annotate_msg_stats_report1(Customers::[tuple()]) -> [tuple()].
annotate_msg_stats_report1(Customers) ->
	lists:map(
		fun(Customer) ->
			{CustomerId, Networks} = Customer,
			[
				{id, CustomerId},
				{networks,
					lists:map(
						fun({NetworkId, MsgIds}) ->
							[
								{id, NetworkId},
								{mids, MsgIds}
							]
						end,
						Networks)}
			]
		end,
		Customers).

-spec annotate_msg_stats_report2(Networks::[tuple()]) -> [tuple()].
annotate_msg_stats_report2(Networks) ->
	lists:map(
		fun({NetworkId, Customers}) ->
			[
				{id, NetworkId},
				{customers,
					lists:map(
						fun({CustomerId, MsgIds}) ->
							[
								{id, CustomerId},
								{mids, MsgIds}
							]
						 end,
						Customers)}
			]
		end,
		Networks).

-spec msg_stats_report1(Records::[tuple()]) -> [tuple()].
msg_stats_report1(Records) ->
	GroupByCustomer = msg_stats_report(1, Records),
	GroupByCustomerAndNetwork = lists:map(fun({K, List}) -> {K, msg_stats_report(1, List)} end, GroupByCustomer),
	GroupByCustomerAndNetwork.

-spec msg_stats_report2(Records::[tuple()]) -> [tuple()].
msg_stats_report2(Records) ->
	GroupByNetwork = msg_stats_report(2, Records),
	GroupByNetworkAndCustomer = lists:map(fun({K, List}) -> {K, msg_stats_report(1, List)} end, GroupByNetwork),
	GroupByNetworkAndCustomer.

-spec msg_stats_report(KeyN::integer(), [tuple()]) -> [tuple()].
msg_stats_report(KeyN, Records) ->
	Pairs = lists:map(fun(R) -> make_pair(KeyN, R) end, Records),
	Dict = lists:foldl(fun({K, V}, Dict) -> orddict:append_list(K, [V], Dict) end, orddict:new(), Pairs),
	List = orddict:to_list(Dict),
	List.

-spec get_msg_stats_file_list(ReportType::integer(), From::os:timestamp(), To::os:timestamp()) -> [file:filename()].
get_msg_stats_file_list(ReportType, From, To) when From < To ->
	Timestamps = get_timestamp_list(From, To),
	lists:map(
		fun(Timestamp) ->
			k_storage_util:msg_stats_file_path(
				io_lib:format("~p-~p.dat", [Timestamp, ReportType]))
		 end, Timestamps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Gateway Stats
%%

-spec gtw_stats_report(Records::[tuple()]) -> [tuple()].
gtw_stats_report(Records) ->
	lists:map(
		fun({gtw_stats, GatewayId, Number, _}) ->
			{GatewayId, Number}
		end, Records).

-spec gtw_stats_report(From::os:timestamp(), To::os:timestamp()) -> {ok, term()} | {error, Reason::any()}.
gtw_stats_report(From, To) when From < To ->
	{FromFloor, ToCeiling} = align_time_range(From, To),
	Timestamps = get_timestamp_list(FromFloor, ToCeiling),
	Reports =
		lists:foldr(
			fun(Timestamp, SoFar) ->
				Filename = k_storage_util:gtw_stats_file_path(io_lib:format("~p.dat", [Timestamp])),
				case k_storage_util:read_term_from_file(Filename) of
					{ok, []} ->
						SoFar;
					{ok, Records} ->
						AnnotatedRecords = annotate_gtw_stats_report(Timestamp, Records),
						[AnnotatedRecords | SoFar];
					{error, _Reason} ->
						%?log_debug("Missing gtw stats report: ~p", [Filename])
						SoFar
				end
			end, [], Timestamps),
	AnnotatedReport = {slices, Reports},
	{ok, AnnotatedReport}.

-spec annotate_gtw_stats_report(Timestamp::os:timestamp(), Records::[term()]) -> [term()].
annotate_gtw_stats_report(Timestamp, Records) ->
	Datetime = k_storage_util:datetime_to_iso_8601(
					k_storage_util:unix_epoch_to_datetime(Timestamp)),
	[
		{datetime, Datetime},
		{gateways,
			lists:map(
				fun({GatewayId, Number}) ->
					[
						{id, GatewayId},
						{count, Number}
					]
				end,
				Records)
		}
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Common
%%

-spec get_timestamp_list(From::os:timestamp(), To::os:timestamp()) -> [os:timestamp()].
get_timestamp_list(From, To) when From < To ->
	Step = stats_report_frequency(),
	lists:seq(From, To, Step).

-spec align_time_range(From::os:timestamp(), To::os:timestamp()) ->
	{FromFloor::os:timestamp(), ToCeiling::os:timestamp()}.
align_time_range(From, To) ->
	FromFloor = From - From rem stats_report_frequency(),
	ToCeiling = case To rem stats_report_frequency() of
					0 -> To;
					Rem -> To - Rem + stats_report_frequency()
				end,
	{FromFloor, ToCeiling}.

%% make_pair(2, {a,b,c}) ==> {b,{a,c}}
%% make_pair(1, {a,b}) ==> {a,b}
-spec make_pair(KeyN::integer(), Tuple::tuple()) -> {Key::term(), Value::tuple()} | {Key::term(), Value::term()}.
make_pair(KeyN, Tuple) ->
	Key = element(KeyN, Tuple),
	ValueList = remove(KeyN, tuple_to_list(Tuple)),
	Value = case length(ValueList) of
				1 -> hd(ValueList);
				_ -> list_to_tuple(ValueList)
			end,
	{Key, Value}.

%% remove(2, "abcdef") ==> "acdef"
-spec remove(N::integer(), List::[term()]) -> [term()].
remove(_, []) -> [];
remove(1, [_|T]) -> T;
remove(N, [H|T]) -> [H | remove(N-1, T)].
