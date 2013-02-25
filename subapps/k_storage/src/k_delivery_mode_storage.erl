-module(k_delivery_mode_storage).

-export([
	set_mt_req_info/2,
	set_mt_resp_info/2,
	set_mt_dlr_info/2,

	get_mt_msg_info/1,

	set_mo_msg_info/2
]).

-type selector() :: bson:document().
-type modifier() :: bson:document().
-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_req_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_resp_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_resp_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_dlr_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_dlr_info(Selector, Modifier) ->
	Command = {
		'findandmodify' , <<"mt_messages">>,
		'query'  , Selector,
		'update' , Modifier
	},
	case mongodb_storage:command(k_prev_dynamic_storage, Command) of
		{ok, {value, _, lastErrorObject, {updatedExisting, true, n, 1}, ok, _}} ->
			ok;
		{ok, {value, undefined, ok, _}} ->
			mongodb_storage:upsert(k_curr_dynamic_storage, mt_messages, Selector, Modifier);
		Error ->
			Error
	end.

-spec get_mt_msg_info(selector()) -> {ok, bson:document()} | {error, reason()}.
get_mt_msg_info(Selector) ->
	case mongodb_storage:find_one(k_curr_dynamic_storage, mt_messages, Selector) of
		{ok, Doc} ->
			{ok, Doc};
		{error, no_entry} ->
			mongodb_storage:find_one(k_prev_dynamic_storage, mt_messages, Selector);
		Error ->
			Error
	end.

-spec set_mo_msg_info(selector(), modifier()) -> ok | {error, reason()}.
set_mo_msg_info(Selector, Modifier) ->
	mongodb_storage:upsert(k_curr_dynamic_storage, mo_messages, Selector, Modifier).
