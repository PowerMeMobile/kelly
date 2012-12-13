-module(k_lists).

-export([
	remove/2,
	group/1,
	groupwith/2,
	findwith/2,
	permutations/1,
	make_ranges/1,
	make_frequencies/1,
	make_pair/2
]).

%% remove(2, "abcdef") ==> "acdef"
-spec remove(N::integer(), List::[term()]) -> [term()].
remove(_, []) -> [];
remove(1, [_|T]) -> T;
remove(N, [H|T]) -> [H | remove(N-1, T)].

-spec findwith(fun((A::term()) -> boolean()), [A::term()]) -> {value, A::term()} | false.
findwith(_, []) ->
	false;
findwith(Pred, [H|T]) ->
	case Pred(H) of
		true ->
			{value, H};
		false ->
			findwith(Pred, T)
	end.

%% group("Mississippi") ==> ["M","i","ss","i","ss","i","pp","i"]
-spec group([A]) -> [[A]].
group(List) ->
	groupwith(fun erlang:'=:='/2, List).

-spec groupwith(Eq::fun((A, A) -> boolean()), [A]) -> [[A]].
groupwith(_, []) ->
	[];
groupwith(Eq, [X|XS]) ->
	{YS, ZS} = lists:splitwith(fun(I) -> Eq(X, I) end, XS),
	[[X|YS] | groupwith(Eq, ZS)].

-spec permutations([A]) -> [[A]].
permutations([]) ->
	[[]];
permutations(L) ->
	[[H|T] || H <- L, T <- permutations(L--[H])].

%% make_ranges([1,2,3,4,5]) ==> [{1,2},{2,3},{3,4},{4,5}]
-spec make_ranges([A]) -> [{A, A}].
make_ranges(List) ->
	make_ranges(List, []).
make_ranges([_|[]], Ranges) ->
	lists:reverse(Ranges);
make_ranges([F,S|T], Ranges) ->
	make_ranges([S|T], [{F, S}|Ranges]).

%% make_frequencies([1,2,3,2,3,3]) ==> [{1,1},{2,2},{3,3}]
-spec make_frequencies([A]) -> [{A, pos_integer()}].
make_frequencies(Timestamps) ->
	Groups = group(lists:sort(Timestamps)),
	[{hd(L), length(L)} || L <- Groups].

%% make_pair(2, {a,b,c}) ==> {b,{a,c}}
%% make_pair(1, {a,b}) ==> {a,b}
-spec make_pair(KeyN::integer(), Tuple::tuple()) -> {Key::term(), Value::tuple()} | {Key::term(), Value::term()}.
make_pair(KeyN, Tuple) ->
	Key = element(KeyN, Tuple),
	ValueList = k_lists:remove(KeyN, tuple_to_list(Tuple)),
	Value = case length(ValueList) of
				1 -> hd(ValueList);
				_ -> list_to_tuple(ValueList)
			end,
	{Key, Value}.
