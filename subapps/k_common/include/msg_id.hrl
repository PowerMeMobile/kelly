-ifndef(msg_id_hrl).
-define(msg_id_hrl, included).

%% InputId {customer id, message id} Funnel -> Just
%% OutputId {gateway id, message id} Just -> Funnel
-type msg_id() :: {string(), string()}.

-endif. % msg_id_hrl
