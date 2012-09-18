-ifndef(msg_id_hrl).
-define(msg_id_hrl, included).

%% InputId {customer id, client type, message id} Funnel -> Just
%% OutputId {gateway id, message id} Just -> Funnel
-type input_msg_id() :: {bitstring(), funnel | k1api, bitstring()}.
-type output_msg_id() :: {bitstring(), bitstring()}.
-type msg_id() :: input_msg_id() | output_msg_id().

-endif. % msg_id_hrl
