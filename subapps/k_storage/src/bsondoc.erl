-module(bsondoc).

-export([
	at/2
]).
-export_type([
	value/0
]).

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
	undefined | %% !!! This is the only reason this module exits. Replace `null' with `undefined' !!!
	bson:regex() |
	bson:javascript() |
	atom() |
	integer() |
	bson:mongostamp() |
	bson:minmaxkey().

-spec at(bson:label(), bson:document()) -> value().
at(Label, Document) ->
	case bson:lookup(Label, Document) of
		{} -> undefined;
		{Value} -> Value
	end.
