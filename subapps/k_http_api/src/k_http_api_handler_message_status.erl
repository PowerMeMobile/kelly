-module(k_http_api_handler_message_status).

-behaviour(gen_cowboy_crud).

%% gen_cowboy_crud callbacks
-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include("crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/msg_status.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
	Read = #method_spec{
				path  =[<<"message_status">>, message_id, <<"customer">>, customer_id],
				params = [
					#param{name = message_id, mandatory = true, repeated = false, type = string},
					#param{name = customer_id, mandatory = true, repeated = false, type = string_uuid}
				]},

	{ok, #specs{
		create = undefined,
		read = Read,
		update = undefined,
		delete = undefined
	}}.

read(Params) ->
	?log_debug("Params: ~p", [Params]),
	MsgId = ?gv(message_id, Params),
	CustId = ?gv(customer_id, Params),
	case k_storage:get_msg_status({CustId, MsgId}) of
		{ok, #msg_status{status = Status}} ->
			{ok, {message, [
				{customer_id, CustId},
				{message_id, MsgId},
				{status, Status}
			]}};
		{error, no_entry} ->
			{exception, 'svc0003'}
	end.

create(_Params) ->
	ok.

update(_Params) ->
	ok.

delete(_Params) ->
	ok.
