-module(k_lists).

-export([
	remove/2,
	group/1,
	groupwith/2,
	findwith/2,
	permutations/1
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
