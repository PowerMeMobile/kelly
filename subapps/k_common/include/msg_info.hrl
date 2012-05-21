-ifndef(msg_info_hrl).
-define(msg_info_hrl, included).

-include_lib("k_common/include/JustAsn.hrl").

-type encoding() :: {atom(), atom()}.
-type source_addr() :: #'FullAddr'{}.
-type dest_addr() :: #'FullAddr'{} | #'FullAddrAndRefNum'{}.

-record(msg_info, {
	id :: string(),
	customer_id :: string(),
	type :: atom(),
	encoding :: encoding(),
	body :: binary(),
	source_addr :: source_addr(),
	dest_addr :: dest_addr(),
	registered_delivery :: boolean()
}).

-endif. % msg_info_hrl
