-ifndef(msg_info_hrl).
-define(msg_info_hrl, included).

-type encoding() :: {atom(), atom()}.

-include_lib("alley_dto/include/addr.hrl").

-type src_addr() :: #addr{}.
-type dst_addr() :: #addr{}.
-type delivery_status() ::
	delivered |
	expired |
	deleted |
	undeliverable |
	accepted |
	unknown |
	rejected.

-record(msg_info, {
    client_type :: funnel | k1api,
    customer_id :: binary(),
	in_msg_id :: binary(),
    gateway_id :: binary(),
	out_msg_id :: binary(),
    type :: atom(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean(),
	req_time :: erlang:timestamp(),
	resp_time :: erlang:timestamp(),
	resp_status :: success | failure,
	dlr_time :: erlang:timestamp(),
	dlr_status :: delivery_status()
}).

-define(MSG_STATUS(MsgInfo),
	case {MsgInfo#msg_info.resp_status, MsgInfo#msg_info.dlr_status} of
		{undefined, undefined} ->
			submitted;
		{RespStatus, undefined} ->
			RespStatus;
		{undefined, DlrStatus} ->
			DlrStatus;
		{_dont_care, DlrStatus} ->
			DlrStatus
	end
).

-endif. % msg_info_hrl
