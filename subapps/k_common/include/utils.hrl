-ifndef(utils_hrl).
-define(utils_hrl, 1).

-define(gv(Key, Params), proplists:get_value(Key, Params)).
-define(gv(Key, Params, Default),
	fun() ->
		case ?gv(Key, Params) of
			undefined -> Default;
			Value -> Value
		end
	end()
).

-define(record_to_proplist(Record),
  fun(Val) ->
    Fields = record_info(fields, Record),
    [_Name| Values] = tuple_to_list(Val),
    lists:zip(Fields, Values)
  end
).

-endif.
