-ifndef(customer_hrl).
-define(customer_hrl, included).

-include("storages.hrl").
-include("network_map.hrl").
-include("provider.hrl").

-type originator_id() :: uuid().
-type customer_uuid() :: uuid().
-type customer_id() :: binary(). %% http customer_id | smpp system-type
-type user_id() :: binary(). %% http user_id | smpp system-id
-type connection_type() :: transmitter | receiver | tranceiver | oneapi | soap | mm.
-type originator_state() :: pending | approved | rejected.
-type user_state() :: active | blocked | deactivated.
-type customer_state() :: active | blocked | deactivated.

-record(feature, {
    name :: binary(),
    value :: binary()
}).
-type feature() :: #feature{}.

-record(originator, {
    id                   :: originator_id(),
    address              :: addr(),
    description = <<"">> :: binary(),
    is_default = false   :: boolean(),
    state                :: originator_state()
}).

-record(user, {
    id                    :: user_id(),
    password              :: binary(),
    connection_types      :: [connection_type()],
    features = []         :: [feature()],
    mobile_phone = <<"">> :: binary(),
    first_name   = <<"">> :: binary(),
    last_name    = <<"">> :: binary(),
    company      = <<"">> :: binary(),
    occupation   = <<"">> :: binary(),
    email        = <<"">> :: binary(),
    country      = <<"">> :: binary(),
    language     = <<"">> :: binary(),
    state                 :: user_state()
}).

-record(customer, {
    customer_uuid       :: customer_uuid(),
    customer_id         :: customer_id(),
    name                :: binary(),
    priority            :: integer(),
    rps                 :: integer() | undefined,
    originators = []    :: [originator()],
    network_map_id      :: network_map_id(),
    default_provider_id :: provider_id() | undefined,
    receipts_allowed    :: boolean(),
    no_retry            :: boolean(),
    default_validity    :: binary(),
    max_validity        :: integer(),
    users = []          :: [user()],
    pay_type            :: pay_type(),
    credit              :: float(),
    credit_limit        :: float(),
    language            :: binary(),
    state               :: customer_state()
}).

-type originator()      :: #originator{}.
-type user()            :: #user{}.
-type customer()        :: #customer{}.

-endif.
