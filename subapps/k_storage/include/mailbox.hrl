-ifndef(mailbox_hrl).
-define(mailbox_hrl, included).

-include("msg_info.hrl").

%% ===================================================================
%% Items
%% ===================================================================

-type item_id() :: binary().

-type incoming_encoding() :: default
                           | gsm033
                           | ascii
                           | latin1
                           | ucs2
                           | integer().

-type incoming_state() :: all
                        | new
                        | read.

-record(k_mb_incoming, {
    id                   :: uuid(),
    customer_uuid        :: customer_uuid(),
    user_id              :: user_id(),
    src_addr             :: addr(),
    dst_addr             :: addr(),
    rcv_time             :: erlang:timestamp(), %% oneapi retrieve sms request
    body                 :: binary(),
    encoding             :: incoming_encoding(),
    state                :: incoming_state(),

    delivery_attempt = 1 :: integer()
}).

-record(k_mb_oneapi_receipt, {
    id                   :: uuid(),
    customer_uuid        :: customer_uuid(),
    user_id              :: user_id(),
    src_addr             :: addr(),
    dst_addr             :: addr(),
    req_id               :: binary(),
    in_msg_id            :: binary(),
    submit_date          :: erlang:timestamp(),
    done_date            :: erlang:timestamp(),
    status               :: status(),

    delivery_attempt = 1 :: integer()
}).

-record(k_mb_funnel_receipt, {
    id                   :: uuid(),
    customer_uuid        :: customer_uuid(),
    user_id              :: user_id(),
    src_addr             :: addr(),
    dst_addr             :: addr(),
    req_id               :: binary(),
    in_msg_id            :: binary(),
    submit_date          :: erlang:timestamp(),
    done_date            :: erlang:timestamp(),
    status               :: status(),

    delivery_attempt = 1 :: integer()
}).

-type k_mb_item() ::
    #k_mb_oneapi_receipt{} |
    #k_mb_funnel_receipt{} |
    #k_mb_incoming{}.

%% ===================================================================
%% Subscriptions
%% ===================================================================

-record(k_mb_oneapi_receipt_sub, {
    id              :: uuid(),
    customer_uuid   :: customer_uuid(),
    user_id         :: user_id(),
    queue_name      :: binary(),
    src_addr        :: addr(),
    notify_url      :: binary(),
    callback_data   :: binary(),
    req_id          :: req_id(),
    in_msg_ids      :: [in_msg_id()],
    created_at      :: erlang:timestamp()
}).

-record(k_mb_oneapi_incoming_sub, {
    id              :: uuid(),
    customer_uuid   :: customer_uuid(),
    user_id         :: user_id(),
    priority        :: integer(),
    queue_name      :: binary(),
    dst_addr        :: addr(),
    notify_url      :: binary(),
    criteria        :: binary(),
    callback_data   :: binary(),
    created_at      :: erlang:timestamp()
}).

-record(k_mb_funnel_sub, {
    id              :: uuid(),
    customer_uuid   :: customer_uuid(),
    user_id         :: user_id(),
    priority        :: integer(),
    queue_name      :: binary(),
    created_at      :: erlang:timestamp()
}).

-type k_mb_subscription() ::
    #k_mb_oneapi_receipt_sub{} |
    #k_mb_oneapi_incoming_sub{} |
    #k_mb_funnel_sub{}.

-endif.
