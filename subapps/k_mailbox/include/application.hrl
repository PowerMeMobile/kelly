-ifndef(k_mailbox_application_hrl).
-define(k_mailbox_application_hrl, included).

-include_lib("k_storage/include/customer.hrl").
-include_lib("k_storage/include/msg_info.hrl").

-define(APP, k_mailbox).

%% ===================================================================
%% Pending Items
%% ===================================================================

-record(k_mb_pending_item, {
    id          :: binary(),
    type        :: atom(),
    customer_id :: binary(),
    user_id     :: binary()
}).

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
    id                   :: binary(),
    customer_id          :: binary(),
    user_id              :: binary(),
    src_addr             :: addr(),
    dst_addr             :: addr(),
    received             :: erlang:timestamp(), %% k1api retrieve sms request
    body                 :: binary(),
    encoding             :: incoming_sms_encoding(),

    delivery_attempt = 1 :: integer(),
    created_at           :: erlang:timestamp()
}).

-record(k_mb_k1api_receipt, {
    id                   :: binary(),
    customer_id          :: binary(),
    user_id              :: binary(),
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
    id                   :: binary(),
    customer_id          :: binary(),
    user_id              :: binary(),
    src_addr          :: addr(),
    dst_addr            :: addr(),
    req_id               :: binary(),
    in_msg_id            :: binary(),
    submit_date          :: erlang:timestamp(),
    done_date            :: erlang:timestamp(),
    status               :: status(),

    delivery_attempt = 1 :: integer(),
    created_at           :: erlang:timestamp()
}).

-type k_mb_item() ::
    #k_mb_k1api_receipt{} |
    #k_mb_funnel_receipt{} |
    #k_mb_incoming_sms{}.

%% ===================================================================
%% Subscriptions
%% ===================================================================

-record(k_mb_k1api_receipt_sub, {
    id              :: binary(),
    customer_id     :: binary(),
    user_id         :: binary(),
    queue_name      :: binary(),
    src_addr        :: addr(),
    client_correlator :: binary(),
    notify_url      :: binary(),
    callback_data   :: binary(),
    req_id          :: req_id(),
    in_msg_ids      :: [in_msg_id()],
    created_at      :: erlang:timestamp()
}).

-record(k_mb_k1api_incoming_sms_sub, {
    id              :: binary(),
    customer_id     :: binary(),
    user_id         :: binary(),
    priority        :: integer(),
    queue_name      :: binary(),
    dst_addr        :: addr(),
    notify_url      :: binary(),
    criteria        :: binary(),
    callback_data   :: binary(),
    created_at      :: erlang:timestamp()
}).

-record(k_mb_funnel_sub, {
    id              :: binary(),
    customer_id     :: binary(),
    user_id         :: binary(),
    priority        :: integer(),
    queue_name      :: binary(),
    created_at      :: erlang:timestamp()
}).

-type k_mb_subscription() ::
    #k_mb_k1api_receipt_sub{} |
    #k_mb_k1api_incoming_sms_sub{} |
    #k_mb_funnel_sub{}.

-endif. % k_mailbox_application_hrl
