-module(k_mailbox_mnesia_init).

-behaviour(gen_server).

-export([
	start_link/0
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").

-include("connection.hrl").
-include("pending_item.hrl").
-include("address.hrl").

-record(state, {}).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	ok = k_mnesia_schema:ensure_table(
		k_mb_connection,
		record_info(fields, k_mb_connection)),
	ok = k_mnesia_schema:ensure_table(
		k_mb_pending_item,
		record_info(fields, k_mb_pending_item)),
	ok = k_mnesia_schema:ensure_table(
		k_mb_address,
		record_info(fields, k_mb_address)
	),
	{ok, #state{}}.

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
