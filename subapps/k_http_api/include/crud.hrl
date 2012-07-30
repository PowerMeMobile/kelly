-ifndef(crud_hrl).
-define(crud_hrl, included).

-type req() :: term().
-type rest_params() :: tuple().
-type reason() :: term().
-type response() :: term().
-type http_method() :: 'GET' | 'POST' | 'PUT' | 'DELETE'.
-type path() :: [binary()].
-type state() :: term().

-record(param, {
	name	 :: atom(),
	mandatory = false :: boolean(),
	repeated = false :: boolean(),
	type	 :: atom()
}).

-record(method_spec, {
	path :: list(),
	params :: [#param{}]
}).

-record(specs, {
	create :: #method_spec{},
	read :: #method_spec{},
	update :: #method_spec{},
	delete :: #method_spec{}
}).

-define(record_to_proplist(Record),
	fun(Val) ->
		Fields = record_info(fields, Record),
		[_Name| Values] = tuple_to_list(Val),
		lists:zip(Fields, Values)
	end
).

-define(gv(Key, List),
	proplists:get_value(Key, List)).

-define(resolve(Key, List, ActualValue),
	fun(Key, List, ActualValue) ->
		TmpValue = ?gv(Key, List),
		case TmpValue of
			undefined -> ActualValue;
			_ -> TmpValue
		end
	end).

resolve(Key, List, ActualValue) ->
	TmpValue = ?gv(Key, List),
		case TmpValue of
			undefined -> ActualValue;
			_ -> TmpValue
		end.

-endif.
