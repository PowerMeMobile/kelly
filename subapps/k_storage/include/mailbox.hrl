-ifndef(mailbox_hrl).
-define(mailbox_hrl, included).

-include("msg_info.hrl").

%% ===================================================================
%% Items
%% ===================================================================

-type item_id() :: binary().

-type incoming_sms_encoding() ::
    default |
    gsm033 |
    ascii |
    latin1 |
    ucs2 |
    integer().

-record(k_mb_incoming_sms, {
    id                   :: uuid(),
    customer_uuid        :: customer_uuid(),
    user_id              :: user_id(),
    src_addr             :: addr(),
    dst_addr             :: addr(),
    received             :: erlang:timestamp(), %% oneapi retrieve sms request
    body                 :: binary(),
    encoding             :: incoming_sms_encoding(),

    delivery_attempt = 1 :: integer(),
    created_at           :: erlang:timestamp()
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

    delivery_attempt = 1 :: integer(),
    created_at           :: erlang:timestamp()
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

    delivery_attempt = 1 :: integer(),
    created_at           :: erlang:timestamp()
}).

-type k_mb_item() ::
    #k_mb_oneapi_receipt{} |
    #k_mb_funnel_receipt{} |
    #k_mb_incoming_sms{}.

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

-record(k_mb_oneapi_incoming_sms_sub, {
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
    #k_mb_oneapi_incoming_sms_sub{} |
    #k_mb_funnel_sub{}.

-endif.
