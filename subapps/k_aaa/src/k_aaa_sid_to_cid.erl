-module(k_aaa_sid_to_cid).

%% API
-export([
	sid_to_cid/2,
	cid_by_sid/1,
	del_sid/1
]).

-include_lib("k_common/include/storages.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec sid_to_cid(system_id(), customer_id()) -> ok | {error, term()}.
sid_to_cid(SystemId, CustomerId) ->
	k_gen_storage_common:write(sid_to_cid, SystemId, CustomerId).

-spec cid_by_sid(system_id()) -> {ok, customer_id()} | {error, term()}.
cid_by_sid(SystemId) ->
	k_gen_storage_common:read(sid_to_cid, SystemId).

-spec del_sid(system_id()) -> ok | {error, term()}.
del_sid(SystemId) ->
	k_gen_storage_common:delete(sid_to_cid, SystemId).
