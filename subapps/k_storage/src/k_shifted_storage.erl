-module(k_shifted_storage).

-export([
	get_mt_msg_info/3,
	get_mo_msg_info/2
]).

-include("application.hrl").
-include_lib("k_common/include/msg_id.hrl").
-include_lib("k_common/include/msg_info.hrl").
-include_lib("k_common/include/customer.hrl").

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec get_mt_msg_info(customer_id(), funnel | k1api, msg_id()) -> {ok, #msg_info{}} | {error, reason()}.
get_mt_msg_info(CustomerId, ClientType, InMsgId) ->
	Selector = {
		'ci'  , CustomerId,
		'ct'  , ClientType,
		'imi' , InMsgId,
		'rqt' , {'$exists', true}
	},
	case find_one(mt_messages, Selector) of
		{ok, Doc} ->
			{ok, k_storage_utils:doc_to_mt_msg_info(Doc)};
		Error ->
			Error
	end.

-spec get_mo_msg_info(binary(), any()) -> {ok, #msg_info{}} | {error, reason()}.
get_mo_msg_info(GatewayId, InMsgId) ->
	Selector = {
		'gi'  , GatewayId,
		'imi' , InMsgId
	},
	case find_one(mo_messages, Selector) of
		{ok, Doc} ->
			{ok, k_storage_utils:doc_to_mo_msg_info(Doc)};
		Error ->
			Error
	end.

%% ===================================================================
%% Internal
%% ===================================================================

find_one(Coll, Selector) ->
	{ok, Shifts} = k_storage_events_manager:get_shifts(),
	find_one(Shifts, Coll, Selector).

find_one([], _Coll, _Selector) ->
	{error, no_entry};
find_one([ShiftDbName|Shifts], Coll, Selector) ->
	{ok, Props} = application:get_env(?APP, shifted_storage),
	MongoDbProps = [{mongodb_dbname, ShiftDbName} | Props],

	{ok, Pid} = mongodb_storage:start_link(MongoDbProps),

	Res = mongodb_storage:find_one(Pid, Coll, Selector),

	ok = mongodb_storage:stop(Pid),

	case Res of
		{ok, _} ->
			Res;
		{error, no_entry} ->
			find_one(Shifts, Coll, Selector)
	end.
