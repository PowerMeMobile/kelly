-module(k_mnesia_schema).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	get_nodes/0,
	ensure_table/2
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

-include("logging.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	nodes
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

get_nodes() ->
	{ok, Nodes} = gen_server:call(?MODULE, get_nodes),
	Nodes.

ensure_table(Tab, RecordInfo) ->
	ok = case mnesia:create_table(Tab, [
					{disc_copies, get_nodes()},
					{attributes, RecordInfo}]) of
			{atomic, ok} ->
				ok;
			{aborted, {already_exists, Tab}} ->
				ok
		 end,
    ok = mnesia:wait_for_tables([Tab], infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init({}) ->
	Nodes = [node()],
	mnesia:set_debug_level(verbose),
	mnesia:stop(),
	mnesia:create_schema(Nodes),
	mnesia:start(),

	{ok, #state{
		nodes = Nodes
	}}.

handle_call(get_nodes, _From, State = #state{nodes = Nodes}) ->
	{reply, {ok, Nodes}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
