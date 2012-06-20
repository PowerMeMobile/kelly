-ifndef(gen_cowboy_restful_spec).
-define(gen_cowboy_restful_spec, included).

%% Type defenitions
-type req() :: term().
-type rest_params() :: tuple().
-type reason() :: term().
-type response() :: term(). %% expect proplist
-type http_method() :: 'GET' | 'POST' | 'PUT' | 'DELETE'. %% expect proplist
-type path() :: [binary()].
-type state() :: term().

-spec init(req(), http_method(), path()) -> {ok, rest_params(), state()}.
-spec handle(req(), rest_params(), state()) -> {ok, response()} | {error, reason()}.
-spec terminate(req(), state()) -> ok.

-define(record_to_proplist(Record),
	fun(Val) ->
		Fields = record_info(fields, Record),
		[Name| Values] = tuple_to_list(Val),
		{Name, lists:zip(Fields, Values)}
	end
).

-define(record_to_proplist_(Record),
	fun(Val, Proplist) ->
		Fields = record_info(fields, Record),
		[Name| Values] = tuple_to_list(Val),
		{Name, Proplist ++ lists:zip(Fields, Values)}
	end
).


-endif. % gen_cowboy_restful_spec
