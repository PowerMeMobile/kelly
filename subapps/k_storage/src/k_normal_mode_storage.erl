-module(k_normal_mode_storage).

-export([
	set_mt_req_info/2,
	set_mt_resp_info/2,
	set_mt_dlr_info_and_get_msg_info/2,

	set_mo_msg_info/2
]).

-type selector() :: bson:document().
-type modifier() :: bson:document().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_req_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_resp_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_resp_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_dlr_info_and_get_msg_info(selector(), modifier()) -> {ok, bson:document()} | {error, reason()}.
set_mt_dlr_info_and_get_msg_info(Selector, Modifier) ->
	Command = {
		'findandmodify', <<"mt_messages">>,
		'query', Selector,
		'update', Modifier,
		'new', true
	},
	case mongodb_storage:command(k_curr_dynamic_storage, Command) of
		{ok, {value, Doc, lastErrorObject, {updatedExisting, true, n, 1}, ok, _}} ->
			{ok, Doc};
		{ok, {value, undefined, ok, _}} ->
			{error, no_entry};
		Error ->
			Error
	end.

-spec set_mo_msg_info(selector(), modifier()) -> ok | {error, reason()}.
set_mo_msg_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mo_messages, Selector, Modifier).
