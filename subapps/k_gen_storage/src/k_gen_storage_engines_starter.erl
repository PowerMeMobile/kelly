-module(k_gen_storage_engines_starter).

-behaviour(gen_server).

%% API
-export([
	start_link/0
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	{ok, Engines} = application:get_env(?APP, engines),
	lists:foreach(
		fun(Engine) ->
			?log_debug("starting ~p...", [Engine]),
			%% this assumes the Engine's name and its application name are the same.
			ok = ensure_app_started(Engine),
			{ok, _} = start_engine(Engine),
			?log_debug("~p started.", [Engine])
		end,
		Engines),
	gen_server:cast(self(), terminate),

	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(terminate, State = #state{}) ->
	{stop, normal, State};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec ensure_app_started(Application::atom()) -> ok | {error, term()}.
ensure_app_started(Application) ->
	case application:start(Application) of
		ok ->
			ok;
		{error, {not_started, Dependency}} ->
			case ensure_app_started(Dependency) of
				ok ->
					ensure_app_started(Application);
				Error ->
					Error
			end;
		{error, {already_started, Application}} ->
			ok;
		Error ->
			Error
	end.

-spec start_engine(Engine::atom()) -> {ok, pid()}.
start_engine(Engine) ->
	%% this assumes the Engine's supervisor's name suffix is `_sup'.
	EngineSup = list_to_atom(atom_to_list(Engine) ++ "_sup"),
	ChildSpec = {Engine,
					{EngineSup, start_link, []},
					permanent, infinity, supervisor, [EngineSup]},
	supervisor:start_child(k_gen_storage_engines_sup, ChildSpec).
