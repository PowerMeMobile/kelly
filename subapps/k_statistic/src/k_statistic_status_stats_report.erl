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
		if (this.dlr_status) {
			emit(this.dlr_status, 1);
		} else if (this.resp_status) {
			emit(this.resp_status, 1);
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
		  'query' , { 'req_time' , { '$gte' , From, '$lt' , To } },
		  'map' , MtMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	MoCommand =
		{ 'mapreduce' , <<"mo_messages">>,
		  'query' , { 'req_time' , { '$gte' , From, '$lt' , To } },
		  'map' , MoMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	{ok, MtBson} = mongodb_storage:command(mt_messages, MtCommand),
	{ok, MoBson} = mongodb_storage:command(mo_messages, MoCommand),
	ResultsMtBson = bson:at(results, MtBson),
	ResultsMoBson = bson:at(results, MoBson),
	ResultsBson = lists:sort(ResultsMtBson ++ ResultsMoBson),
	Results = [
	 	{list_to_existing_atom(binary_to_list(Status)), round(Hits)} || {'_id', Status, value, Hits} <- ResultsBson
	 ],
	{ok, {statuses, Results}}.

-spec get_report(From::erlang:timestamp(), To::erlang:timestamp(), Status::status()) -> [any()].
get_report(From, To, received) ->
	Selector = [ { 'req_time' , { '$gte' , From, '$lt' , To } } ],
	get_raw_report(mo_messages, Selector);

get_report(From, To, submitted) ->
	Selector = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'resp_status' , { '$exists' , false } },
		{ 'dlr_status' , { '$exists' , false } }
	],
	get_raw_report(mt_messages, Selector);

get_report(From, To, Status) when
	Status == success; Status == failure
->
	Selector = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'resp_status' , Status }
	],
	get_raw_report(mt_messages, Selector);

get_report(From, To, Status) when
	Status == enroute; Status == delivered; Status == expired;
	Status == deleted; Status == undeliverable; Status == accepted;
	Status == unknown; Status == rejected; Status == unrecognized
->
	Selector = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'dlr_status' , Status }
	],
	get_raw_report(mt_messages, Selector).

%% ===================================================================
%% Internal
%% ===================================================================

get_raw_report(Collection, Selector) ->
	case mongodb_storage:find(Collection, Selector) of
		{ok, List} ->
			{ok, {messages,
				[prettify_plist(Plist) || {_Id, Plist} <- List]
			}};
		Error ->
			Error
	end.

prettify_plist(Plist) ->
	InMsgId = proplists:get_value(in_msg_id, Plist),
	GatewayId = proplists:get_value(gateway_id, Plist),
	CustomerId = proplists:get_value(customer_id, Plist),
	Type = proplists:get_value(type, Plist),
	Encoding = proplists:get_value(encoding, Plist),
	Body = proplists:get_value(body, Plist),
	SrcAddrDoc = proplists:get_value(src_addr, Plist),
	DstAddrDoc = proplists:get_value(dst_addr, Plist),
	ReqTime = proplists:get_value(req_time, Plist),

	Datetime = list_to_binary(
		k_datetime:datetime_to_iso_8601(
			k_datetime:unix_epoch_to_datetime(
				k_datetime:timestamp_to_unix_epoch(ReqTime)))),
	[
		{datetime, Datetime},
		{message_id, InMsgId},
		{gateway_id, GatewayId},
		{customer_id, CustomerId},
		{src_addr, addr_to_proplist(k_storage:doc_to_addr(SrcAddrDoc))},
		{dst_addr, addr_to_proplist(k_storage:doc_to_addr(DstAddrDoc))},
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
