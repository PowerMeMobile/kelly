-ifndef(gen_consumer_spec).
-define(gen_consumer_spec, included).

-type gen_srv_start_args() :: term().
-type gen_srv_stop_reason() :: term().
-type gen_srv_timeout() :: integer() | infinity.
-type gen_srv_state() :: term().
-type gen_srv_reply() :: term().
-type gen_srv_request() :: term().
-type gen_srv_from() :: {pid(),reference()}.
-type gen_srv_vsn() :: term() | {down, term()}.
-type gen_srv_extra() :: term().

-type gen_srv_init_result() :: 
		{ok,gen_srv_state()} 
	|	{ok,gen_srv_state(),gen_srv_timeout()}
	|	{ok,gen_srv_state(),hibernate}
	|	{stop,gen_srv_stop_reason()}
	|	ignore.

-spec init(gen_srv_start_args()) -> gen_srv_init_result().


-type gen_srv_handle_call_result() :: 
		{reply,gen_srv_reply(),gen_srv_state()} 
	|	{reply,gen_srv_reply(),gen_srv_state(),gen_srv_timeout()}
 	|	{reply,gen_srv_reply(),gen_srv_state(),hibernate}
	|	{noreply,gen_srv_state()}
	|	{noreply,gen_srv_state(),gen_srv_timeout()}
	|	{noreply,gen_srv_state(),hibernate}
	|	{stop,gen_srv_stop_reason(),gen_srv_reply(),gen_srv_state()}
	|	{stop,gen_srv_stop_reason(),gen_srv_state()}.

-spec handle_call(gen_srv_request(), gen_srv_from(), gen_srv_state()) -> gen_srv_handle_call_result().


-type gen_srv_handle_cast_result() ::
		{noreply,gen_srv_state()}
	|	{noreply,gen_srv_state(),gen_srv_timeout()}
	|	{noreply,gen_srv_state(),hibernate}
	|	{stop,gen_srv_stop_reason(),gen_srv_state()}.

-spec handle_cast(gen_srv_request(), gen_srv_state()) -> gen_srv_handle_cast_result().

-type gen_srv_handle_info_result() ::
		{noreply,gen_srv_state()}
	|	{noreply,gen_srv_state(),gen_srv_timeout()} 
	|	{noreply,gen_srv_state(),hibernate}
	|	{stop,gen_srv_stop_reason(),gen_srv_state()}.

-spec handle_info(gen_srv_request(), gen_srv_state()) -> gen_srv_handle_info_result().

-spec terminate(gen_srv_stop_reason(), gen_srv_state()) -> ignored.

-spec code_change(gen_srv_vsn(), gen_srv_state(), gen_srv_extra()) -> {ok, gen_srv_state()}.

-type channel() :: pid().
-type content_type() :: binary().
-type payload() :: binary().
-type amqp_opts() :: [{Key :: atom(), Value :: term()}].
-type queue_name() :: binary().
-type exchange_name() :: binary().

-spec handle_message(content_type(), payload(), channel(), gen_srv_state()) -> {noreply, pid(), gen_srv_state()} | {noreply, gen_srv_state()}.
-spec handle_subscribe(gen_srv_state()) -> {ok, amqp_opts(), queue_name(), exchange_name(), gen_srv_state()} | {ok, amqp_opts(), queue_name(), gen_srv_state()}.
-spec handle_consume_ok(gen_srv_state()) -> {noreply, gen_srv_state()}.
-spec handle_cancel_ok(gen_srv_state()) -> {noreply, gen_srv_stop_reason(), gen_srv_state()}.

-endif. % gen_consumer_spec