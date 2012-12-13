-module(k_statistic_status_stats_report).

-export([
	get_report/2,
	get_report/3
]).

-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/msg_status.hrl").

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
		{ 'mapreduce' , <<"outgoing_messages">>,
		  'query' , { 'req_time' , { '$gte' , From, '$lt' , To } },
		  'map' , MtMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	MoCommand =
		{ 'mapreduce' , <<"incoming_messages">>,
		  'query' , { 'req_time' , { '$gte' , From, '$lt' , To } },
		  'map' , MoMapF,
		  'reduce' , ReduceF,
		  'out' , { 'inline' , 1 }
		},
	{ok, MtBson} = mongodb_storage:command(outgoing_messages, MtCommand),
	{ok, MoBson} = mongodb_storage:command(incoming_messages, MoCommand),
	ResultsMtBson = bson:at(results, MtBson),
	ResultsMoBson = bson:at(results, MoBson),
	ResultsBson = lists:sort(ResultsMtBson ++ ResultsMoBson),
	Results = [
	 	{Status, round(Hits)} || {'_id', Status, value, Hits} <- ResultsBson
	 ],
	{ok, {statuses, Results}}.

-spec get_report(From::erlang:timestamp(), To::erlang:timestamp(), Status::status()) -> [any()].
get_report(From, To, received) ->
	Selectors = [ { 'req_time' , { '$gte' , From, '$lt' , To } } ],
	get_raw_report(incoming_messages, Selectors);

get_report(From, To, submitted) ->
	Selectors = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'resp_status' , { '$exists' , false } },
		{ 'dlr_status' , { '$exists' , false } }
	],
	get_raw_report(outgoing_messages, Selectors);

get_report(From, To, Status) when
	Status == success; Status == failure
->
	Selectors = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'resp_status' , Status }
	],
	get_raw_report(outgoing_messages, Selectors);

get_report(From, To, Status) when
	Status == accepted; Status == deleted; Status == delivered;
	Status == expired; Status == rejected; Status == undeliverable;
	Status == unknown
->
	Selectors = [
		{ 'req_time' , { '$gte' , From, '$lt' , To } },
		{ 'dlr_status' , Status }
	],
	get_raw_report(outgoing_messages, Selectors).

%% ===================================================================
%% Internal
%% ===================================================================

get_raw_report(Collection, Selectors) ->
	case mongodb_storage:find(Collection, Selectors) of
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

addr_to_proplist(FAddr = #addr{ref_num = undefined}) ->
	#addr{
		addr = Addr,
		ton = Ton,
		npi = Npi
	} = FAddr,

	[{addr, Addr},
	{ton, Ton},
	{npi, Npi}];

addr_to_proplist(FAddr = #addr{}) ->
	#addr{
		addr = Addr,
		ton = Ton,
		npi = Npi,
		ref_num = RefNum
	} = FAddr,

	[{addr, Addr},
	{ton, Ton},
	{npi, Npi},
	{ref_num, RefNum}].
