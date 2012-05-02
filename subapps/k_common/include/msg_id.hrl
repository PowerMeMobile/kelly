-ifndef(msg_id_hrl).
-define(msg_id_hrl, included).

%% InputId {customer id, message id}
%% OuputId {gateway id, message id}
-type msg_id() :: {string(), string()}.

-endif. % msg_id_hrl
