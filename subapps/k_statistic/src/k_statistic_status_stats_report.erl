-module(k_statistic_status_stats_report).

-export([
	get_report/2,
	get_report/3
]).

-include_lib("k_common/include/msg_info.hrl").

-type status() ::
	received
  | submitted
  | resp_status()
  | dlr_status().

%% ===================================================================
%% API
%% ===================================================================

-spec get_report(From::erlang:timestamp(), To::erlang:timestamp()) -> [any()].
get_report(From, To) ->
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
	MtCommand =
		{ 'mapreduce' , <<"mt_messages">>,
		  'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		  'map' , MtMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	MoCommand =
		{ 'mapreduce' , <<"mo_messages">>,
		  'query' , { 'rqt' , { '$gte' , From, '$lt' , To } },
		  'map' , MoMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	{ok, MtBson} = mongodb_storage:command(k_curr_dynamic_storage, MtCommand),
	{ok, MoBson} = mongodb_storage:command(k_curr_dynamic_storage, MoCommand),
	ResultsMtBson = bson:at(results, MtBson),
	ResultsMoBson = bson:at(results, MoBson),
	ResultsBson = lists:sort(ResultsMtBson ++ ResultsMoBson),
	Results = [
	 	{list_to_existing_atom(binary_to_list(Status)), round(Hits)} || {'_id', Status, value, Hits} <- ResultsBson
	 ],
	{ok, {statuses, Results}}.

-spec get_report(From::erlang:timestamp(), To::erlang:timestamp(), Status::status()) -> [any()].
get_report(From, To, received) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		}
	},
	get_raw_report(mo_messages, Selector);

get_report(From, To, submitted) ->
	Selector = {
		'rqt' , {
			'$gte' , From,
			'$lt'  , To
		},
		'rps' , { '$exists' , false },
		'ds'  , { '$exists' , false }
	},
	get_raw_report(mt_messages, Selector);

get_report(From, To, Status) when
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

get_report(From, To, Status) when
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
	case mongodb_storage:find(k_curr_dynamic_storage, Collection, Selector) of
		{ok, Docs} ->
			{ok, {messages,
				[doc_to_message(Doc) || {_Id, Doc} <- Docs]
			}};
		Error ->
			Error
	end.

doc_to_message(Doc) ->
	InMsgId = bson:at(imi, Doc),
	GatewayId = bson:at(gi, Doc),
	CustomerId = bson:at(ci, Doc),
	Type = bson:at(t, Doc),
	Encoding = bson:at(e, Doc),
	Body = bson:at(b, Doc),
	SrcAddrDoc = bson:at(sa, Doc),
	DstAddrDoc = bson:at(da, Doc),
	ReqTime = bson:at(rqt, Doc),

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
