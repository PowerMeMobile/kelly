-ifndef(msisdn_hrl).
-define(msisdn_hrl, included).

-include("storages.hrl").
-include("customer.hrl").

-type msisdn() :: addr().
-type state() :: all | free | used.

-record(msisdn_info, {
    msisdn :: msisdn(),
    customer_uuid :: customer_uuid(),
    user_id :: user_id()
}).

-endif.
