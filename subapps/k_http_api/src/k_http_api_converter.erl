-module(k_http_api_converter).

-define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-export([process/2]).

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
	Converted = xml_preprocess(Response),
	?log_debug("Converted: ~p", [Converted]),
	Xml = xmerl:export_simple(Converted, xmerl_xml,[{prolog, ?xml_prolog}]),
	?log_debug("Xml: ~p", [Xml]),
	BinXml = unicode:characters_to_binary(Xml),
	{ok, BinXml}.

xml_preprocess(Atom) when is_atom(Atom) ->
	[atom_to_list(Atom)];
xml_preprocess(Integer) when is_integer(Integer) ->
	[integer_to_list(Integer)];
xml_preprocess(List = [{_Key, _Value} | _Rest]) ->
	[{K, xml_preprocess(V)} || {K, V} <- List];
xml_preprocess(ObjectList = [ [{_K,_V} | _RestObjectProps] | _RestObjectList]) ->
	[{item, xml_preprocess(Object)} || Object <- ObjectList];
xml_preprocess(List = [String | _Rest]) when is_list(String) ->
	[{item, [Item]} || Item <- List];
xml_preprocess({K, V}) ->
	[{K, xml_preprocess(V)}];
xml_preprocess(List = [Item | _Rest]) when is_atom(Item) ->
	[{item, [atom_to_list(Atom)]} || Atom <- List];
xml_preprocess(Value) ->
	[Value].

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

