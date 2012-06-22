-module(k_http_api_converter).

-define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-export([process/2, test/0]).

-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

process(Response, _Suff) when is_binary(Response) ->
	{ok, Response};
process(Response, <<"xml">>) ->
	convert_to_xml(Response);
process(Response, <<"json">>) ->
	convert_to_json(Response);
process(Response, _AnySuff) ->
	process(Response, <<"json">>).

%% ===================================================================
%% XML Constructor Functions
%% ===================================================================

convert_to_xml(Response) ->
	?log_debug("Response: ~p", [Response]),
	Converted = preprocess(Response),
	?log_debug("Converted: ~p", [Converted]),
	Xml = xmerl:export_simple(Converted, xmerl_xml,[{prolog, ?xml_prolog}]),
	% ?log_debug("Xml: ~p", [Xml]),
	BinXml = unicode:characters_to_binary(Xml),
	{ok, BinXml}.

%% %% make possible to process List of PropLists
%% xml_preprocess(Response) when is_list(Response) ->
%% 	lists:map(fun(PropList)->
%% 		xml_preprocess(PropList, [])
%% 	end, Response);
%% xml_preprocess(Response) ->
%% 	xml_preprocess([Response]).


%% %% converting proplist to xmerl compatible format
%% 	%% in case of Value = []
%% xml_preprocess({Name, []}, []) ->
%% 	{Name, ["undefined"]};
%% 	%%
%% xml_preprocess({Name, []}, Acc) ->
%% 	{Name, lists:reverse(Acc)};
%% 	%% make possible to chek list in entire depth
%% xml_preprocess({Name, [{SubName, SubValue} | Tail]}, Acc) ->
%% 	Sub = xml_preprocess({SubName, SubValue}, []),
%% 	%%  is not tail recursion
%% 	xml_preprocess({Name, Tail}, [Sub | Acc]);
%% 	%% only atom converts
%% xml_preprocess({Name, [Value | Tail]}, []) when is_atom(Value) ->
%% 	AtomString = convert_atom_list([Value | Tail]),
%% 	{Name, AtomString};
%%  	%% for such values as ["string1", "string2"]
%% xml_preprocess({Name, String = [Value | Tail]}, []) when is_list(Value) ->
%% 	StringList = string:join(String, ","),
%% 	{Name, [StringList]};
%% 	%% for such values as {atom, "string"}
%% xml_preprocess({Name, Value}, []) when is_list(Value) ->
%% 	{Name, [Value]};
%% 	%% for other values such as integer
%% xml_preprocess({Name, Value}, Acc) ->
%% 	xml_preprocess({Name, []}, [io_lib:format("~p", [Value]) | Acc]).

%% xml_preprocess(List = [{K, V} | Rest]) ->
%% 	{item, List}
%% xml_preprocess(List = [Item | Rest]) when is_list(Item) ->


%% xml_preprocess({K, V})  ->
%% 	[{K, xml_preprocess(V)}].

preprocess([]) ->
	[];
%preprocess{[List | Rest]}
preprocess({K, V}) ->
	[{K,preprocess(V)}].


%% json_preprocess(null) ->
%% 	null;
%% json_preprocess(undefined) ->
%% 	null;
%% json_preprocess(true) ->
%% 	true;
%% json_preprocess(false) ->
%% 	false;
%% json_preprocess(Binary) when is_binary(Binary) ->
%% 	Binary;
%% json_preprocess(Number) when is_integer(Number) orelse
%% 							  is_float(Number) ->
%% 	Number;
%% json_preprocess(Atom) when is_atom(Atom) ->
%% 	atom_to_binary(Atom, utf8);

%% preprocess(List = [{_K, _V} | _Rest]) ->
%% 	[{Key, preprocess(Value)} || {Key, Value} <- List];
%% 	%% for such values as ["string1", "string2"]
%% preprocess(List = [E | _Rest]) when is_list(E) ->
%% 	?log_debug("List: ~p", [List]),
%% 	[Element || Element <- List];
%% preprocess(List = [E | _Rest]) when is_atom(E) ->
%% 	[preprocess(Element) || Element <- List];
%% preprocess(List) when is_list(List) ->
%% 	list_to_binary(List).


test() ->
	Object = {networks, [
		[{id, id}]
	]},
	Converted = preprocess(Object),
	?log_debug("Converted: ~p", [Converted]),
	Xml = xmerl:export_simple(Converted, xmerl_xml,[{prolog, ?xml_prolog}]),
	% ?log_debug("Xml: ~p", [Xml]),
	BinXml = unicode:characters_to_binary(Xml),
	?log_debug("BinXml: ~p", [BinXml]).

%% help to convert [atom1, atom2] to "atom1,atom2"
convert_atom_list(AtomList) ->
        convert_atom_list(AtomList, []).

convert_atom_list([], Acc) ->
		% ?log_debug("Acc: ~p", [Acc]),
        Result = string:join(lists:append(Acc), ","),
        % ?log_debug("Result: ~p", [Result]),
        [Result];
convert_atom_list([Atom | Tail], Acc) ->
        convert_atom_list(Tail, [io_lib:format("~p", [Atom]) | Acc]).

%% ===================================================================
%% JSON Constructor Functions
%% ===================================================================

convert_to_json(Object) ->
	?log_debug("Object: ~p", [Object]),
	PreProcessedObject = json_preprocess(Object),
	?log_debug("PreProcessedObject: ~p", [PreProcessedObject]),
	Json = jsx:term_to_json(PreProcessedObject),
	?log_debug("Json: ~p", [Json]),
	{ok, Json}.

%% preprocess object before convert to json
json_preprocess({K, V}) ->
	json_preprocess([{K,V}]);
json_preprocess(null) ->
	null;
json_preprocess(undefined) ->
	null;
json_preprocess(true) ->
	true;
json_preprocess(false) ->
	false;
json_preprocess(Binary) when is_binary(Binary) ->
	Binary;
json_preprocess(Number) when is_integer(Number) orelse
							  is_float(Number) ->
	Number;
json_preprocess(Atom) when is_atom(Atom) ->
	atom_to_binary(Atom, utf8);
json_preprocess(List = [{_K, _V} | _Rest]) ->
	[{json_preprocess(Key), json_preprocess(Value)} || {Key, Value} <- List];
	%% for such values as ["string1", "string2"]
json_preprocess(List = [E | _Rest]) when is_list(E) ->
	?log_debug("List: ~p", [List]),
	[json_preprocess(Element) || Element <- List];
json_preprocess(List = [E | _Rest]) when is_atom(E) ->
	[json_preprocess(Element) || Element <- List];
json_preprocess(List) when is_list(List) ->
	list_to_binary(List).

