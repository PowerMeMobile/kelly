-module(bsondoc).

-export([
	at/2,
	atom_to_binary/1,
	binary_to_atom/1
]).
-export_type([
	value/0
]).

%% !!! replace `null' with `undefined' !!!
%% !!! get rid of atom() type !!!
-type value() ::
	float() |
	bson:utf8() |
	bson:document() |
	bson:arr() |
	bson:bin() |
	bson:bfunction() |
	bson:uuid() |
	bson:md5() |
	bson:userdefined() |
	bson:objectid() |
	boolean() |
	bson:unixtime() |
	undefined |
	bson:regex() |
	bson:javascript() |
	integer() |
	bson:mongostamp() |
	bson:minmaxkey().

-spec at(bson:label(), bson:document()) -> value().
at(Label, Document) ->
	case bson:lookup(Label, Document) of
		{} -> undefined;
		{Value} -> Value
	end.

-spec atom_to_binary(atom()) -> binary().
atom_to_binary(Atom) ->
	atom_to_binary(Atom, utf8).

-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Bin) when is_binary(Bin) ->
	binary_to_atom(Bin, utf8);
binary_to_atom(Atom) when is_atom(Atom) ->
	Atom.
