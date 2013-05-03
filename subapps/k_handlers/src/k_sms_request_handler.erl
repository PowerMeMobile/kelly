-module(k_sms_request_handler).

-export([process/2]).
-compile({no_auto_import, [split_binary/2]}).

-include("amqp_worker_reply.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("k_mailbox/include/application.hrl").

-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

-define(gv(Key, Params), proplists:get_value(Key, Params)).
-define(gv(Key, Params, Default), proplists:get_value(Key, Params, Default)).

-record(port_addressing, {
	dst_port :: non_neg_integer(),
	src_port :: non_neg_integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec process(binary(), binary()) -> {ok, [#worker_reply{}]} | {error, any()}.
process(_ContentType, Message) ->
	case adto:decode(#just_sms_request_dto{}, Message) of
		{ok, SmsReq} ->
			process_sms_request(SmsReq);
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec process_sms_request(#just_sms_request_dto{}) -> {ok, [#worker_reply{}]} | {error, any()}.
process_sms_request(#just_sms_request_dto{client_type = ClientType} = SmsReq) ->
	?log_debug("Got ~p sms request: ~p", [ClientType, SmsReq]),
	ReqTime = k_datetime:utc_timestamp(),
	ReqInfos = sms_request_to_req_info_list(SmsReq, ReqTime),
	case ClientType of
		k1api ->
			InMsgIds = [InMsgId || #req_info{in_msg_id = InMsgId} <- ReqInfos],
			process_k1api_req(SmsReq, InMsgIds);
		_ ->
			nop
	end,
	case k_utils:safe_foreach(
		fun k_dynamic_storage:set_mt_req_info/1, ReqInfos, ok, {error, '_'}
	) of
		ok ->
			{ok, []};
		Error ->
			Error
	end.

-spec sms_request_to_req_info_list(#just_sms_request_dto{}, erlang:timestamp()) -> [#req_info{}].
sms_request_to_req_info_list(SmsReq, ReqTime) ->
	#just_sms_request_dto{
		type = Type,
		dest_addrs = {Type, DstAddrs},
		message_ids = MsgIds
	} = SmsReq,
	Pairs = lists:zip(DstAddrs, MsgIds),
	build_req_infos(SmsReq, ReqTime, Pairs).

build_req_infos(SmsReq, ReqTime, Pairs) ->
	build_req_infos(SmsReq, ReqTime, Pairs, []).

build_req_infos(_, _, [], Acc) ->
	lists:reverse(Acc);
build_req_infos(SmsReq, ReqTime, [{DstAddr, MsgId}|Pairs], Acc) ->
	Type = SmsReq#just_sms_request_dto.type,
	ReqInfo =
		case request_type(Type, MsgId) of
			short ->
				[build_short_req_info(SmsReq, ReqTime, DstAddr, MsgId)];
			{long, MsgIds} ->
				lists:reverse(build_long_req_infos(SmsReq, ReqTime, DstAddr, MsgIds));
			part ->
				[build_part_req_info(SmsReq, ReqTime, DstAddr, MsgId)]
		end,
	build_req_infos(SmsReq, ReqTime, Pairs, ReqInfo ++ Acc).

build_short_req_info(#just_sms_request_dto{
	id = ReqId,
	client_type = ClientType,
	customer_id = CustomerId,
	user_id = UserId,
	gateway_id = GatewayId,
	type = Type,
	message = Body,
	encoding = Encoding,
	params = Params,
	source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId) ->
	RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
	EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
	ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
	#req_info{
		req_id = ReqId,
		client_type = ClientType,
		customer_id = CustomerId,
		user_id = UserId,
		in_msg_id = InMsgId,
		gateway_id = GatewayId,
		type = Type,
		encoding = Encoding,
		body = Body,
		src_addr = SrcAddr,
		dst_addr = DstAddr,
		reg_dlr = RegDlr,
		esm_class = EsmClass,
		val_period = ValPeriod,
		req_time = ReqTime
	}.

build_part_req_info(#just_sms_request_dto{
	id = ReqId,
	client_type = ClientType,
	customer_id = CustomerId,
	user_id = UserId,
	gateway_id = GatewayId,
	type = Type,
	message = Body,
	encoding = Encoding,
	params = Params,
	source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId) ->
	PartRefNum = DstAddr#addr.ref_num,
	PartRefNum = get_param_by_name(<<"sar_msg_ref_num">>, Params, undefined),
	PartSeqNum = get_param_by_name(<<"sar_segment_seqnum">>, Params, undefined),
	PartsTotal = get_param_by_name(<<"sar_total_segments">>, Params, undefined),

	RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
	EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
	ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
	#req_info{
		req_id = ReqId,
		client_type = ClientType,
		customer_id = CustomerId,
		user_id = UserId,
		in_msg_id = InMsgId,
		gateway_id = GatewayId,
		type = Type,
		encoding = Encoding,
		body = Body,
		part_ref_num = PartRefNum,
		part_seq_num = PartSeqNum,
		parts_total = PartsTotal,
		src_addr = SrcAddr,
		dst_addr = DstAddr#addr{ref_num = undefined},
		reg_dlr = RegDlr,
		esm_class = EsmClass,
		val_period = ValPeriod,
		req_time = ReqTime
	}.

build_long_req_infos(SmsReq, ReqTime, DstAddr, InMsgIds) ->
	Body = SmsReq#just_sms_request_dto.message,
	Encoding = SmsReq#just_sms_request_dto.encoding,
	Params = SmsReq#just_sms_request_dto.params,
    {_Encoding, _DC, Bitness} = encoding_dc_bitness(Encoding, Params, default_gateway_settings()),
	PortAddressing = port_addressing(Params),

	%% when a concatenated message comes its reference number is not really needed.
	%% the reference number comes only from Funnel, but not from SOAP and OneAPI.
	%% in fact, its absence gives a good idea that it's possible to make up the whole
	%% message body by querying the current part's request id and destination address.
	%% this should return all parts. now sort the parts by sequence number and
	%% concatenate the bodies.
	PartRefNum = undefined,

	PartsTotal = length(InMsgIds),
	PartSeqNums = lists:seq(1, PartsTotal),
	BodyParts = split_msg(Body, Bitness, PortAddressing),
	[
		build_long_part_req_info(
			SmsReq, ReqTime, DstAddr, InMsgId, BodyPart, PartRefNum, PartSeqNum, PartsTotal
		) || {InMsgId, BodyPart, PartSeqNum} <- lists:zip3(InMsgIds, BodyParts, PartSeqNums)
	].

build_long_part_req_info(#just_sms_request_dto{
	id = ReqId,
	client_type = ClientType,
	customer_id = CustomerId,
	user_id = UserId,
	gateway_id = GatewayId,
	encoding = Encoding,
	params = Params,
	source_addr = SrcAddr
}, ReqTime, DstAddr, InMsgId, BodyPart, PartRefNum, PartSeqNum, PartsTotal) ->
	RegDlr = get_param_by_name(<<"registered_delivery">>, Params, false),
	EsmClass = get_param_by_name(<<"esm_class">>, Params, 0),
	ValPeriod = get_param_by_name(<<"validity_period">>, Params, <<"">>),
	#req_info{
		req_id = ReqId,
		client_type = ClientType,
		customer_id = CustomerId,
		user_id = UserId,
		in_msg_id = InMsgId,
		gateway_id = GatewayId,
		type = part,
		encoding = Encoding,
		body = BodyPart,
		part_ref_num = PartRefNum,
		part_seq_num = PartSeqNum,
		parts_total = PartsTotal,
		src_addr = SrcAddr,
		dst_addr = DstAddr,
		reg_dlr = RegDlr,
		esm_class = EsmClass,
		val_period = ValPeriod,
		req_time = ReqTime
	}.

-spec get_param_by_name(binary(), [#just_sms_request_param_dto{}], term()) -> term().
get_param_by_name(Name, Params, Default) ->
	case lists:keyfind(Name, #just_sms_request_param_dto.name, Params) of
		false ->
			Default;
		#just_sms_request_param_dto{value = {_, Value}} ->
			Value
	end.

process_k1api_req(#just_sms_request_dto{
	client_type = k1api,
	id = RequestId,
	customer_id = CustomerId,
	user_id = _UserId,
	params = Params,
	source_addr = SourceAddr
}, InMsgIds) ->
	InputMessageIds = [{CustomerId, k1api, InMsgId} || InMsgId <- InMsgIds],
	NotifyURL = get_param_by_name(<<"k1api_notify_url">>, Params, undefined),
	?log_debug("NotifyURL: ~p", [NotifyURL]),
	CallbackData = get_param_by_name(<<"k1api_callback_data">>, Params, undefined),
	?log_debug("CallbackData: ~p", [CallbackData]),
	SubId = create_k1api_receipt_subscription(CustomerId, <<"undefined">>, SourceAddr, NotifyURL, CallbackData),
	ok = link_input_id_to_sub_id(InputMessageIds, SubId),
	ok = link_sms_request_id_to_message_ids(CustomerId, undefined, SourceAddr, RequestId, InputMessageIds);
process_k1api_req(_, _) ->
	ok.

create_k1api_receipt_subscription(_, _, _, undefined, _) -> undefined;
create_k1api_receipt_subscription(CustomerId, UserId, DestAddr, NotifyURL, CallbackData) ->
	QName = <<"pmm.k1api.incoming">>,
	SubscriptionId = uuid:unparse(uuid:generate_time()),
	Subscription = #k_mb_k1api_receipt_sub{
		id = SubscriptionId,
		customer_id = CustomerId,
		user_id = UserId,
		queue_name = QName,
		dest_addr = DestAddr,
		notify_url = NotifyURL,
		callback_data = CallbackData,
		created_at = k_datetime:utc_timestamp()
	},
	ok = k_mailbox:register_sms_req_receipts_subscription(Subscription),
	SubscriptionId.

link_input_id_to_sub_id(_, undefined) -> ok;
link_input_id_to_sub_id([], _SubId) -> ok;
link_input_id_to_sub_id([InputId | RestIds], SubId) ->
	ok = k_mb_db:link_input_id_to_sub_id(InputId, SubId),
	link_input_id_to_sub_id(RestIds, SubId).

link_sms_request_id_to_message_ids(
	CustomerId, UserId, SenderAddress, SmsRequestId, InputMessageIds
) ->
	%% Include CustomerId & UserId to Key to avoid access to another's
	%% sms statuses
	%% Include SenderAddress into Key to make SmsRequestId unique
	%% within specific SenderAddress
	ok = k_k1api:link_sms_request_id_to_msg_ids(CustomerId, UserId, SenderAddress, SmsRequestId, InputMessageIds).

%% ===================================================================
%% Message splitting logic from Just
%% ===================================================================

default_gateway_settings() -> [
	{default_encoding, gsm0338},
	{default_data_coding, 0},
	{default_bitness, 7}
].

request_type(Type, MsgId) ->
	case {Type, binary:split(MsgId, <<":">>, [global, trim])} of
		{regular, [MsgId]} ->
			short;
		{regular, MsgIds} ->
			{long, MsgIds};
		{part, [MsgId]} ->
			part
	end.

encoding_dc_bitness(Encoding, Params, Settings) ->
	{E, DC, B} =
		case Encoding of
			default -> {
				?gv(default_encoding, Settings),
				?gv(default_data_coding, Settings),
				?gv(default_bitness, Settings)
			};
			gsm0338 ->
				{gsm0338, 0, 7};
			ascii ->
				{ascii, 1, 7};
			latin1 ->
				{latin1, 3, 8};
			ucs2 ->
				{ucs2, 8, 16};
			Other ->
				{other, Other, 8}
		end,
	{E, ?gv(data_coding, Params, DC), B}.

port_addressing(Params) ->
	DstPort = get_param_by_name(<<"destination_port">>, Params, undefined),
	SrcPort = get_param_by_name(<<"source_port">>, Params, undefined),
	case DstPort =/= undefined andalso SrcPort =/= undefined of
		true ->
			#port_addressing{dst_port = DstPort, src_port = SrcPort};
		false ->
			undefined
	end.

max_msg_len(Bitness, undefined) ->
	case Bitness of
		7  -> {160, 153};
		8  -> {140, 134};
		16 -> {140, 134}
	end;
max_msg_len(Bitness, _) ->
	case Bitness of
		7  -> {152, 146};
		8  -> {133, 128};
		16 -> {132, 128}
	end.

split_msg(Msg, Bitness, PortAddressing) ->
	{MaxWhole, MaxPart} = max_msg_len(Bitness, PortAddressing),
	case size(Msg) > MaxWhole of
		true  ->
			split_binary(Msg, MaxPart);
		false ->
			[Msg]
	end.

-spec split_binary(binary(), pos_integer()) -> [binary()].
split_binary(Bin, Len) when Len > 0 ->
	split_binary(Bin, Len, []).

split_binary(Bin, Len, Acc) ->
	case size(Bin) of
		0 ->
			lists:reverse(Acc);
		N when N =< Len ->
			lists:reverse([Bin | Acc]);
		_ ->
			{Bin1, Bin2} = erlang:split_binary(Bin, Len),
			split_binary(Bin2, Len, [Bin1 | Acc])
	end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

sms_request_to_req_info_list_regular_short_batch_test() ->
	RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
	GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
	CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
	UID = <<"user">>,
	CT = funnel,
	Body = <<"Hello">>,
	Encoding = default,
	ReqTime = {0,0,0},

	SmsReq = #just_sms_request_dto{
		id = RID,
		gateway_id = GID,
		customer_id = CID,
		user_id = UID,
		client_type = CT,
		type = regular,
		message = Body,
   		encoding = Encoding,
		params = [
			#just_sms_request_param_dto{
				name = <<"registered_delivery">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"service_type">>,
				value = {string,<<>>}
			},
			#just_sms_request_param_dto{
				name = <<"no_retry">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"validity_period">>,
				value = {string,<<"000003000000000R">>}
			},
			#just_sms_request_param_dto{
				name = <<"priority_flag">>,
				value = {integer,0}
			},
			#just_sms_request_param_dto{
				name = <<"esm_class">>,
				value = {integer,3}
			},
			#just_sms_request_param_dto{
				name = <<"protocol_id">>,
				value = {integer,0}
			}
		],
		source_addr = #addr{addr = <<"0">>},
		dest_addrs = {regular,[
			#addr{addr = <<"1">>},
			#addr{addr = <<"2">>}
		]},
		message_ids = [<<"1">>, <<"2">>]
	},
	Expected = [
		#req_info{
		   req_id = RID,
		   client_type = CT,
		   customer_id = CID,
		   user_id = UID,
		   in_msg_id = <<"1">>,
		   gateway_id = GID,
		   type = regular,
		   encoding = Encoding,
		   body = Body,
		   src_addr = #addr{addr = <<"0">>},
		   dst_addr = #addr{addr = <<"1">>},
		   reg_dlr = false,
		   esm_class = 3,
		   val_period = <<"000003000000000R">>,
		   req_time = ReqTime
		},
		#req_info{
		   req_id = RID,
		   client_type = CT,
		   customer_id = CID,
		   user_id = UID,
		   in_msg_id = <<"2">>,
		   gateway_id = GID,
		   type = regular,
		   encoding = Encoding,
		   body = Body,
		   src_addr = #addr{addr = <<"0">>},
		   dst_addr = #addr{addr = <<"2">>},
		   reg_dlr = false,
		   esm_class = 3,
		   val_period = <<"000003000000000R">>,
		   req_time = ReqTime
		}
	],
	?assertEqual(Expected, sms_request_to_req_info_list(SmsReq, ReqTime)).

sms_request_to_req_info_list_part_test() ->
	RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
	GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
	CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
	UID = <<"user">>,
	CT = funnel,
	Body = <<"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
	Encoding = default,
	ReqTime = {0,0,0},

	SmsReq = #just_sms_request_dto{
		id = RID,
		gateway_id = GID,
		customer_id = CID,
		user_id = UID,
		client_type = CT,
		type = part,
		message = Body,
   		encoding = Encoding,
		params = [
			#just_sms_request_param_dto{
				name = <<"data_coding">>,
				value = {integer,240}
			},
			#just_sms_request_param_dto{
				name = <<"sar_msg_ref_num">>,
				value = {integer,249}
			},
			#just_sms_request_param_dto{
				name = <<"sar_total_segments">>,
				value = {integer,3}
			},
			#just_sms_request_param_dto{
				name = <<"sar_segment_seqnum">>,
				value = {integer,1}
			},
			#just_sms_request_param_dto{
				name = <<"registered_delivery">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"service_type">>,
				value = {string,<<>>}
			},
			#just_sms_request_param_dto{
				name = <<"no_retry">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"validity_period">>,
				value = {string,<<"000003000000000R">>}
			},
			#just_sms_request_param_dto{
				name = <<"priority_flag">>,
				value = {integer,0}
			},
			#just_sms_request_param_dto{
				name = <<"esm_class">>,
				value = {integer,0}
			},
			#just_sms_request_param_dto{
				name = <<"protocol_id">>,
				value = {integer,0}
			}
		],
		source_addr = #addr{addr = <<"0">>},
		dest_addrs = {part,[
			#addr{addr = <<"1">>, ref_num = 249}
		]},
		message_ids = [<<"1">>]
	},
	Expected = [
		#req_info{
			req_id = RID,
			client_type = CT,
			customer_id = CID,
			user_id = UID,
			in_msg_id = <<"1">>,
			gateway_id = GID,
			type = part,
			encoding = Encoding,
			body = Body,
			part_ref_num = 249,
			part_seq_num = 1,
			parts_total = 3,
			src_addr = #addr{addr = <<"0">>},
			dst_addr = #addr{addr = <<"1">>},
			reg_dlr = false,
			esm_class = 0,
			val_period = <<"000003000000000R">>,
			req_time = ReqTime
		}
	],
	Actual = sms_request_to_req_info_list(SmsReq, ReqTime),
	?assertEqual(Expected, Actual).

sms_request_to_req_info_list_multipart_test() ->
	RID = <<"bad506f0-b2fa-11e2-a1ef-00269e42f7a5">>,
	GID = <<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>,
	CID = <<"feda5822-5271-11e1-bd27-001d0947ec73">>,
	UID = <<"user">>,
	CT = funnel,
	Body = <<"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222223333333">>,
	Encoding = default,
	ReqTime = {0,0,0},

	SmsReq = #just_sms_request_dto{
		id = RID,
		gateway_id = GID,
		customer_id = CID,
		user_id = UID,
		client_type = CT,
		type = regular,
		message = Body,
   		encoding = Encoding,
		params = [
			#just_sms_request_param_dto{
				name = <<"sar_msg_ref_num">>,
				value = {integer,249}
			},
			#just_sms_request_param_dto{
				name = <<"registered_delivery">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"service_type">>,
				value = {string,<<>>}
			},
			#just_sms_request_param_dto{
				name = <<"no_retry">>,
				value = {boolean,false}
			},
			#just_sms_request_param_dto{
				name = <<"validity_period">>,
				value = {string,<<"000003000000000R">>}
			},
			#just_sms_request_param_dto{
				name = <<"priority_flag">>,
				value = {integer,0}
			},
			#just_sms_request_param_dto{
				name = <<"esm_class">>,
				value = {integer,0}
			},
			#just_sms_request_param_dto{
				name = <<"protocol_id">>,
				value = {integer,0}
			}
		],
		source_addr = #addr{addr = <<"0">>},
		dest_addrs = {regular,[
			#addr{addr = <<"1">>}
		]},
		message_ids = [<<"1:2:3">>]
	},
	Expected = [
		#req_info{
			req_id = RID,
			client_type = CT,
			customer_id = CID,
			user_id = UID,
			in_msg_id = <<"1">>,
			gateway_id = GID,
			type = part,
			encoding = Encoding,
			body = <<"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
			part_ref_num = undefined,
			part_seq_num = 1,
			parts_total = 3,
			src_addr = #addr{addr = <<"0">>},
			dst_addr = #addr{addr = <<"1">>},
			reg_dlr = false,
			esm_class = 0,
			val_period = <<"000003000000000R">>,
			req_time = ReqTime
		},
		#req_info{
			req_id = RID,
			client_type = CT,
			customer_id = CID,
			user_id = UID,
			in_msg_id = <<"2">>,
			gateway_id = GID,
			type = part,
			encoding = Encoding,
			body = <<"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222">>,
			part_ref_num = undefined,
			part_seq_num = 2,
			parts_total = 3,
			src_addr = #addr{addr = <<"0">>},
			dst_addr = #addr{addr = <<"1">>},
			reg_dlr = false,
			esm_class = 0,
			val_period = <<"000003000000000R">>,
			req_time = ReqTime
		},
		#req_info{
			req_id = RID,
			client_type = CT,
			customer_id = CID,
			user_id = UID,
			in_msg_id = <<"3">>,
			gateway_id = GID,
			type = part,
			encoding = Encoding,
			body = <<"3333333">>,
			part_ref_num = undefined,
			part_seq_num = 3,
			parts_total = 3,
			src_addr = #addr{addr = <<"0">>},
			dst_addr = #addr{addr = <<"1">>},
			reg_dlr = false,
			esm_class = 0,
			val_period = <<"000003000000000R">>,
			req_time = ReqTime
		}
	],
	Actual = sms_request_to_req_info_list(SmsReq, ReqTime),
	?assertEqual(Expected, Actual).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
