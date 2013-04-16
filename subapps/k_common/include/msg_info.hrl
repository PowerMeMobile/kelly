-ifndef(msg_info_hrl).
-define(msg_info_hrl, included).

-include("customer.hrl").
-include_lib("alley_dto/include/addr.hrl").
-include_lib("alley_dto/include/adto_types.hrl").

-type resp_status() ::
	sent
  | failed.

-type dlr_status() ::
	enroute
  | delivered
  | expired
  | deleted
  | undeliverable
  | accepted
  | unknown
  | rejected
  | unrecognized.

-type status() ::
	received
  | pending
  | resp_status()
  | dlr_status().

-type req_id() :: binary().
-type in_msg_id() :: binary().
-type out_msg_id() :: binary().
-type encoding() :: atom() | integer().
-type src_addr() :: #addr{}.
-type dst_addr() :: #addr{}.
-type timestamp() :: erlang:timestamp().
-type msg_id() :: binary(). %% <<req_id()/binary, $#, in_msg_id()>>

-record(req_info, {
	req_id :: req_id(),
    client_type :: client_type(),
    customer_id :: customer_id(),
	user_id :: user_id(),
	in_msg_id :: in_msg_id(),
    gateway_id :: gateway_id(),
    type :: atom(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean(),
	req_time :: timestamp()
}).

-record(resp_info, {
	req_id :: req_id(),
    client_type :: client_type(),
    customer_id :: customer_id(),
	in_msg_id :: in_msg_id(),
    gateway_id :: gateway_id(),
	out_msg_id :: out_msg_id(),
	resp_time :: timestamp(),
	resp_status :: resp_status()
}).

-record(dlr_info, {
    gateway_id :: gateway_id(),
	out_msg_id :: out_msg_id(),
	dlr_time :: timestamp(),
	dlr_status :: dlr_status()
}).

-record(msg_info, {
	msg_id :: msg_id(),
    client_type :: client_type(),
    customer_id :: customer_id(),
	user_id :: user_id(),
	in_msg_id :: in_msg_id(),
    gateway_id :: gateway_id(),
	out_msg_id :: out_msg_id(),
    type :: atom(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean(),
	status :: status(),
	req_time :: timestamp(),
	resp_time :: timestamp(),
	dlr_time :: timestamp()
}).

-define(MSG_STATUS(MsgInfo),
	case MsgInfo#msg_info.status of
		success -> sent;
		failure -> failed;
		Status -> Status
	end
).

-define(MAKE_MSG_ID(ReqId, InMsgId), <<ReqId/binary, $#, InMsgId/binary>>).

-endif. % msg_info_hrl
