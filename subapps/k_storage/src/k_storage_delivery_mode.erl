-module(k_delivery_mode_storage).

-export([
	set_mt_req_info/2,
	set_mt_resp_info/2,
	set_mt_dlr_info_and_get_msg_info/3,
	set_mt_downlink_dlr_status/2,

	set_mo_msg_info/2,
	set_mo_downlink_dlr_status/2
]).

-type selector() :: bson:document().
-type sort() :: bson:document().
-type modifier() :: bson:document().
-type reason() :: no_entry | term().

%% ===================================================================
%% API
%% ===================================================================

-spec set_mt_req_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_req_info(Selector, Modifier) ->
	mongodb_storage:upsert(curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_resp_info(selector(), modifier()) -> ok | {error, reason()}.
set_mt_resp_info(Selector, Modifier) ->
	mongodb_storage:upsert(curr_dynamic_storage, mt_messages, Selector, Modifier).

-spec set_mt_dlr_info_and_get_msg_info(selector(), sort(), modifier()) ->
	{ok, bson:document()} | {error, reason()}.
set_mt_dlr_info_and_get_msg_info(Selector, Sort, Modifier) ->
	Command = {
		'findandmodify', <<"mt_messages">>,
		'query' , Selector,
		'sort'  , Sort,
		'update', Modifier,
		'new'   , true
	},
	case mongodb_storage:command(prev_dynamic_storage, Command) of
		{ok, PrevResult} ->
			case bsondoc:at(value, PrevResult) of
				undefined ->
					case mongodb_storage:command(curr_dynamic_storage, Command) of
						{ok, CurrResult} ->
							case bsondoc:at(value, CurrResult) of
								undefined ->
									{error, no_entry};
								Doc ->
									{ok, Doc}
							end;
						Error ->
							Error
					end;
				Doc ->
					{ok, Doc}
			end;
		Error ->
			Error
	end.

-spec set_mt_downlink_dlr_status(selector(), modifier()) -> ok | {error, reason()}.
set_mt_downlink_dlr_status(Selector, Modifier) ->
	Command = {
		'findandmodify', <<"mt_messages">>,
		'query', Selector,
		'update', Modifier
	},
	case mongodb_storage:command(prev_dynamic_storage, Command) of
		{ok, Result} ->
			case bsondoc:at(value, Result) of
				undefined ->
					mongodb_storage:upsert(curr_dynamic_storage, mt_messages, Selector, Modifier);
				_ ->
					ok
			end;
		Error ->
			Error
	end.

-spec set_mo_msg_info(selector(), modifier()) -> ok | {error, reason()}.
set_mo_msg_info(Selector, Modifier) ->
	mongodb_storage:upsert(curr_dynamic_storage, mo_messages, Selector, Modifier).

-spec set_mo_downlink_dlr_status(selector(), modifier()) -> ok | {error, reason()}.
set_mo_downlink_dlr_status(Selector, Modifier) ->
	mongodb_storage:upsert(curr_dynamic_storage, mo_messages, Selector, Modifier).
