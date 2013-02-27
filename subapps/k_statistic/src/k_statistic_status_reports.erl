-module(k_statistic_status_reports).

-export([
	get_mt_msg_status_report/3,
	get_aggregated_statuses_report/2,
	get_msgs_by_status_report/3
]).

-include_lib("k_common/include/msg_info.hrl").

-type customer_id() :: binary().
-type client_type() :: funnel | k1api.
-type in_msg_id() :: binary().
-type report() :: term().
-type reason() :: term().
-type timestamp() :: erlang:timestamp().

-type status() ::
	received
  | submitted
  | resp_status()
  | dlr_status().

%% ===================================================================
%% API
%% ===================================================================

-spec get_mt_msg_status_report(customer_id(), client_type(), in_msg_id()) -> {ok, report()} | {error, reason()}.
get_mt_msg_status_report(CustomerId, ClientType, InMsgId) ->
	Selector = {
		'ci'  , CustomerId,
		'ct'  , ClientType,
		'imi' , InMsgId,
		'rqt' , {'$exists', true}
	},
	case k_shifted_storage:find_one(mt_messages, Selector) of
		{ok, Doc} ->
			MsgInfo = k_storage_utils:doc_to_mt_msg_info(Doc),
			{ok, {
				message, [
					{customer_id, CustomerId},
					{message_id, InMsgId},
					{client_type, ClientType},
					{status, ?MSG_STATUS(MsgInfo)}
				]
			}};
		Error ->
			Error
	end.

-spec get_aggregated_statuses_report(timestamp(), timestamp()) -> {ok, report()} | {error, reason()}.
get_aggregated_statuses_report(From, To) ->
	MtMapF =
<<"
	function() {
		if (this.ds) {
			emit(this.ds, 1);
		} else if (this.rps) {
			emit(this.rps, 1);
		} else {
			emit(\"submitted\", 1);
		}
	};
">>,
	MoMapF =
<<"
	function() {
		emit(\"received\", 1);
	};
">>,
	ReduceF =
<<"
	function(key, values) {
		return Array.sum(values);
	};
">>,
	MtCommand = {
		'mapreduce' , <<"mt_messages">>,
		'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		'map' , MtMapF,
		'reduce' , ReduceF,
		'out' , { 'inline' , 1 }
	},
	MoCommand = {
		'mapreduce' , <<"mo_messages">>,
		'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		'map' , MoMapF,
		'reduce' , ReduceF,
		'out' , { 'inline' , 1 }
	},
	{ok, MtDocs} = k_shifted_storage:command(MtCommand),
	{ok, MoDocs} = k_shifted_storage:command(MoCommand),
	Docs = MtDocs ++ MoDocs,
	Results = merge([
	 	{list_to_existing_atom(binary_to_list(Status)), round(Hits)} || {'_id', Status, value, Hits} <- Docs
	 ]),
	{ok, {statuses, Results}}.

merge(Pairs) ->
	dict:to_list(
		merge(Pairs, dict:new())
	).

merge([], Dict) ->
	Dict;
merge([{Key, Value}|Pairs], Dict) ->
	NewDict = dict:update_counter(Key, Value, Dict),
	merge(Pairs, NewDict).

-spec get_msgs_by_status_report(timestamp(), timestamp(), status()) -> {ok, report()} | {error, reason()}.
get_msgs_by_status_report(From, To, received) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		}
	},
	get_raw_report(mo_messages, Selector);

get_msgs_by_status_report(From, To, submitted) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		},
		'rps' , { '$exists' , false },
		'ds'  , { '$exists' , false }
	},
	get_raw_report(mt_messages, Selector);

get_msgs_by_status_report(From, To, Status) when
	Status == success; Status == failure
->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		},
		'rps' , Status
	},
	get_raw_report(mt_messages, Selector);

get_msgs_by_status_report(From, To, Status) when
	Status == enroute; Status == delivered; Status == expired;
	Status == deleted; Status == undeliverable; Status == accepted;
	Status == unknown; Status == rejected; Status == unrecognized
->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt' , To
		},
		'ds' , Status
	},
	get_raw_report(mt_messages, Selector).

%% ===================================================================
%% Internal
%% ===================================================================

get_raw_report(Collection, Selector) ->
	case k_shifted_storage:find(Collection, Selector) of
		{ok, Docs} ->
			{ok, {messages,
				[doc_to_message(Doc) || {_Id, Doc} <- Docs]
			}};
		Error ->
			Error
	end.

doc_to_message(Doc) ->
	InMsgId = bsondoc:at(imi, Doc),
	GatewayId = bsondoc:at(gi, Doc),
	CustomerId = bsondoc:at(ci, Doc),
	Type = bsondoc:at(t, Doc),
	Encoding = bsondoc:at(e, Doc),
	Body = bsondoc:at(b, Doc),
	SrcAddrDoc = bsondoc:at(sa, Doc),
	DstAddrDoc = bsondoc:at(da, Doc),
	ReqTime = bsondoc:at(rqt, Doc),

	Datetime = list_to_binary(
		k_datetime:datetime_to_iso8601(
			k_datetime:unixepoch_to_datetime(
				k_datetime:timestamp_to_unixepoch(ReqTime)))),
	[
		{datetime, Datetime},
		{message_id, InMsgId},
		{gateway_id, GatewayId},
		{customer_id, CustomerId},
		{src_addr, addr_to_proplist(k_storage_utils:doc_to_addr(SrcAddrDoc))},
		{dst_addr, addr_to_proplist(k_storage_utils:doc_to_addr(DstAddrDoc))},
		{type, Type},
		{encoding, Encoding},
		{message_text, Body}
	].

addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = undefined}) ->
	[
		{addr, Addr},
		{ton, Ton},
		{npi, Npi}
	];
addr_to_proplist(#addr{addr = Addr, ton = Ton, npi = Npi, ref_num = RefNum}) ->
	[
		{addr, Addr},
		{ton, Ton},
		{npi, Npi},
		{ref_num, RefNum}
	].
