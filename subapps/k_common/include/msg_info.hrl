-ifndef(msg_info_hrl).
-define(msg_info_hrl, included).

-type encoding() :: {atom(), atom()}.

-record(full_addr, {
	addr :: string(),
	ton :: integer(),
	npi :: integer()
}).

-record(full_addr_ref_num, {
	full_addr :: #full_addr{},
	ref_num :: integer()
}).

-type src_addr() :: #full_addr{}.
-type dst_addr() :: #full_addr{} | #full_addr_ref_num{}.

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
	dlr_status :: delivered | expired | deleted | undeliverable | accepted | unknown | rejected
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
