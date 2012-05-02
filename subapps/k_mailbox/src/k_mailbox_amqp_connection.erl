-module(k_mailbox_amqp_connection).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	get_connection/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("application.hrl").
-include_lib("k_common/include/logging.hrl").
-include_lib("k_common/include/gen_server_spec.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {amqp_conn}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_connection() -> {ok, pid()}.
get_connection() ->
	gen_server:call(?MODULE, get_conn, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, AMQP_Conn} = start_link_amqp_connection(),
    {ok, #state{amqp_conn = AMQP_Conn}}.

handle_call(get_conn, _From, State = #state{
	amqp_conn = AMQP_Conn
}) ->
	{reply, {ok, AMQP_Conn}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

start_link_amqp_connection() ->
	{ok, Params} = application:get_env(?APP, amqp_params),
	Host = proplists:get_value(host, Params, "localhost"),
	Port = proplists:get_value(port, Params, 5672),
	Xchg = proplists:get_value(exchange, Params, <<"/">>),
	User = proplists:get_value(username, Params, <<"guess">>),
	Pass = proplists:get_value(password, Params, <<"guess">>),

	AMQP_Params = #amqp_params_network{
					username = User,
					password = Pass,
					virtual_host = Xchg,
					host = Host,
					port = Port,
					heartbeat = 1
				},
	{ok, AMQP_Conn} = amqp_connection:start(AMQP_Params),
	erlang:link(AMQP_Conn),
	{ok, AMQP_Conn}.
