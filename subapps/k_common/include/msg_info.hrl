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
    id :: binary(),
    customer_id :: binary(),
	% in_msg_id :: binary(),
    gateway_id :: binary(),
	% out_msg_id :: binary(),
    client_type :: funnel | k1api,
    type :: atom(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean()
}).

-endif. % msg_info_hrl
