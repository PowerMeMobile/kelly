-ifndef(msg_info_hrl).
-define(msg_info_hrl, included).

-include("storages.hrl").
-include("customer.hrl").

-type resp_status() ::
    submitted
  | failed
  | blocked.

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

-record(part_info, {
    ref :: integer() | [in_msg_id()],
    seq :: pos_integer(),
    total :: pos_integer()
}).

-type req_id() :: uuid().
-type msg_type() :: regular | {part, #part_info{}}.
-type in_msg_id() :: binary().
-type out_msg_id() :: binary().
-type encoding() :: atom() | integer().
-type src_addr() :: #addr{}.
-type dst_addr() :: #addr{}.
-type timestamp() :: erlang:timestamp().
-type msg_id() :: binary(). %% MongoDB ObjectID
-type esm_class() :: integer().
-type val_period() :: binary().

-record(batch_info, {
    req_id :: req_id(),
    customer_id :: customer_uuid(),
    user_id :: user_id(),
    client_type :: client_type(),
    gateway_id :: gateway_id(),
    body :: binary(),
    src_addr :: src_addr(),
    %reg_dlr :: boolean(),
    %esm_class  :: esm_class(),
    %val_period :: val_period(),
    req_time :: timestamp(),
    recipients :: pos_integer(),
    messages :: pos_integer(),
    price :: float()
}).

-record(req_info, {
    req_id :: req_id(),
    customer_id :: customer_uuid(),
    user_id :: user_id(),
    client_type :: client_type(),
    in_msg_id :: in_msg_id(),
    gateway_id :: gateway_id(),
    type :: msg_type(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean(),
    esm_class  :: esm_class(),
    val_period :: val_period(),
    req_time :: timestamp(),
    network_id :: network_id(),
    price :: float()
}).

-record(resp_info, {
    req_id :: req_id(),
    customer_id :: customer_uuid(),
    in_msg_id :: in_msg_id(),
    client_type :: client_type(),
    gateway_id :: gateway_id(),
    out_msg_id :: out_msg_id(),
    resp_time :: timestamp(),
    resp_status :: resp_status(),
    resp_error_code :: undefined | pos_integer()
}).

-record(dlr_info, {
    gateway_id :: gateway_id(),
    out_msg_id :: out_msg_id(),
    dlr_time :: timestamp(),
    dlr_status :: dlr_status()
}).

-record(msg_info, {
    msg_id :: msg_id(),
    customer_id :: customer_uuid(),
    user_id :: user_id(),
    client_type :: client_type(),
    req_id :: req_id(),
    in_msg_id :: in_msg_id(),
    gateway_id :: gateway_id(),
    out_msg_id :: out_msg_id(),
    type :: msg_type(),
    encoding :: encoding(),
    body :: binary(),
    src_addr :: src_addr(),
    dst_addr :: dst_addr(),
    reg_dlr :: boolean(),
    esm_class  :: esm_class(),
    val_period :: val_period(),
    status :: status(),
    req_time :: timestamp(),
    resp_time :: timestamp(),
    dlr_time :: timestamp(),
    network_id :: network_id(),
    price :: float()
}).

-endif. % msg_info_hrl
