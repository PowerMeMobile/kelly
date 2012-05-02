-module(k_http_api_converter).

-define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-export([process/2]).

-include_lib("k_common/include/logging.hrl").

%% API Functions

process(Response, _Suff) when is_binary(Response) ->
	{ok, Response};
process(Response, <<"xml">>) ->
	convert_to_xml(Response);
process(Response, <<"json">>) ->
	convert_to_json(Response);
process(Response, _AnySuff) ->
	process(Response, <<"xml">>).

%% Internal Functions

convert_to_xml(Response) ->
	?log_debug("Response: ~p", [Response]),
	Converted = xml_preprocess(Response),
	?log_debug("Converted: ~p", [Converted]),
	Xml = xmerl:export_simple(Converted, xmerl_xml,[{prolog, ?xml_prolog}]),
	% ?log_debug("Xml: ~p", [Xml]),
	BinXml = unicode:characters_to_binary(Xml),
	{ok, BinXml}.

%% make possible to process List of PropLists
xml_preprocess(Response) when is_list(Response) ->
	lists:map(fun(PropList)->
		xml_preprocess(PropList, [])
	end, Response);
xml_preprocess(Response) ->
	xml_preprocess([Response]).


%% converting proplist to xmerl compatible format
	%% in case of Value = []
xml_preprocess({Name, []}, []) ->
	{Name, ["undefined"]};
	%%
xml_preprocess({Name, []}, Acc) ->
	{Name, lists:reverse(Acc)};
	%% make possible to chek list in entire depth
xml_preprocess({Name, [{SubName, SubValue} | Tail]}, Acc) ->
	Sub = xml_preprocess({SubName, SubValue}, []),
	%%  is not tail recursion
	xml_preprocess({Name, Tail}, [Sub | Acc]);
	%% only atom converts
xml_preprocess({Name, [Value | Tail]}, []) when is_atom(Value) ->
	AtomString = convert_atom_list([Value | Tail]),
	{Name, AtomString};
 	%% for such values as ["string1", "string2"]
xml_preprocess({Name, String = [Value | Tail]}, []) when is_list(Value) ->
	StringList = string:join(String, ","),
	{Name, [StringList]};
	%% for such values as {atom, "string"}
xml_preprocess({Name, Value}, []) when is_list(Value) ->
	{Name, [Value]};
	%% for other values such as integer
xml_preprocess({Name, Value}, Acc) ->
	xml_preprocess({Name, []}, [io_lib:format("~p", [Value]) | Acc]).

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



convert_to_json(Response) ->
	?log_debug("Response: ~p", [Response]),
	Data = json_preprocess(Response),
	?log_debug("Data: ~p", [Data]),
	Json = rfc4627:encode(Data),
	?log_debug("Json: ~p", [Json]),
	{ok, list_to_binary(Json)}.

%% converts Respose to rfc4627 compatible formart
json_preprocess(Response) ->
	json_preprocess(Response, []).
	%% for such values as "" (empty string as well as empty lists)
json_preprocess({Name, []}, []) ->
	{obj, [{atom_to_list(Name), null}]};
json_preprocess({Name, []}, Acc) ->
	{obj, [{atom_to_list(Name), lists:reverse(Acc)}]};
json_preprocess({Name, [{SubName, SubValue} | Tail]}, Acc) ->
	Sub = json_preprocess({SubName, SubValue}, []),
	json_preprocess({Name, Tail}, [Sub | Acc]);
	%% for such values as ["string1", "string2"]
json_preprocess({Name, [Value | Tail]}, []) when is_list(Value) ->
	StringList = lists:map(fun(String)->
		list_to_binary(String)
	end, [Value | Tail]),
	?log_debug("StringList: ~p", [StringList]),
	{obj, [{atom_to_list(Name), StringList}]};
	%% for such values as [atom1, atom2]
json_preprocess({Name, [Value | Tail]}, []) when is_atom(Value) ->
	AtomList = lists:map(fun(Atom)->
		list_to_binary(io_lib:format("~p", [Atom]))
	end, [Value | Tail]),
	?log_debug("AtomList: ~p", [AtomList]),
	{obj, [{atom_to_list(Name), AtomList}]};
json_preprocess({Name, Value}, []) ->
	{obj, [{atom_to_list(Name), convert_element_js(Value)}]}.


convert_element_js(undefined) ->
	null;
convert_element_js(Element) when is_binary(Element) ->
	Element;
convert_element_js(Element) when is_atom(Element) ->
	Element;
convert_element_js(Element) when is_list(Element) ->
	list_to_binary(Element);
convert_element_js(Element) when is_integer(Element) ->
	Element;
convert_element_js(Element) when is_float(Element) ->
	Element;
convert_element_js(Element) when Element == null ->
	Element;
convert_element_js(Element) when Element == false ->
	Element;
convert_element_js(Element) when Element == true ->
	Element.
% convert_element_js(Element) when is_tuple(Element) ->
% 	list_to_binary(io_lib:format("~p", [Element])).
