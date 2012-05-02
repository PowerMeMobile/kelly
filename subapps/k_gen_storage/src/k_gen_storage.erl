-module(k_gen_storage).

%% API
-export([
	open/1,
	close/1,
	read/2,
	write/3,
	delete/2
]).

%% Behaviour's required callbacks.
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_link, 0}, {open, 2}, {close, 1}, {read, 2}, {write, 3}, {delete, 2}];
behaviour_info(_Other) ->
    undefined.

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").

-type handle() :: {Engine::atom(), Collection::term()}.

%% ===================================================================
%% API
%% ===================================================================

-spec open(CollectionName::term()) -> {ok, Handle::handle()} | {error, Reason::term()}.
open(CollectionName) ->
	{ok, Application} = application:get_application(),
	{ok, {Engine, Options}} = storage_info(Application),
	case Engine:open(CollectionName, Options) of
		{ok, Collection} ->
			{ok, {Engine, Collection}};
		Error ->
			Error
		 end.

-spec close(Handle::handle()) -> ok | {error, Reason::term()}.
close({Engine, Collection}) ->
	Engine:close(Collection).

-spec read(Handle::handle(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read({Engine, Collection}, Key) ->
	Engine:read(Collection, Key).

-spec write(Handle::handle(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write({Engine, Collection},  Key, Value) ->
	Engine:write(Collection, Key, Value).

-spec delete(Handle::handle(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete({Engine, Collection}, Key) ->
	Engine:delete(Collection, Key).

%% ===================================================================
%% Internal
%% ===================================================================

-spec storage_info(atom()) -> {ok, {atom(), [tuple()]}} | {error, no_entry}.
storage_info(Application) ->
	case application:get_env(Application, storage) of
		{ok, Args} ->
			case proplists:get_value(engine, Args) of
				undefined ->
					{error, no_entry};
				Engine ->
					case proplists:get_value(options, Args) of
						undefined ->
							{error, no_entry};
						Options ->
							{ok, {Engine, Options}}
					end
			end;
		undefined ->
			{error, no_entry}
	end.
