#!/usr/bin/env escript
%-module(csv2json).

-export([main/1]).

-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(prefix, {
    network_id                      :: {string, string()},
    prefix                          :: {string, string()}
}).

-record(network, {
    '_id'                           :: {string, string()},
    name                            :: {string, string()},
    country                         :: {string, string()},
    hex_code                        :: {string, string()},
    country_code                    :: {string, string()},
    number_len                      :: {integer, integer()},
    prefixes = {array, []}          :: {array, [{string, string()}]},
    gmt_diff                        :: {string, string()},
    dst                             :: {string, string()},
    provider_id                     :: {string, string()},
    is_home                         :: {boolean, boolean()},
    sms_points                      :: {float, float()},
    sms_mult_credits = {float, 1.0} :: {float, float()}
}).

-define(ROW_DELIM, "\r\n"). % "\r"
-define(COL_DELIM, $,).     % $|
-define(COL_WRAPPER, $").

%% ===================================================================
%% API
%% ===================================================================

-spec main(list()) -> no_return().
main([NetworksFilename, PrefixesFilename]) ->
    {ok, Prefixes} = parse_prefixes_file(PrefixesFilename),
    %io:format("~p~n", [Prefixes]),
    {ok, Networks} = parse_networks_file(NetworksFilename),
    %io:format("~p~n", [Networks]),

    PrefixesDict = build_prefixes_dict(Prefixes),

    Networks2 = set_prefixes_to_networks(Networks, PrefixesDict),
    %io:format("~p~n", [Networks2]),

    Jsons = [proplist_to_json(record_to_proplist(N)) || N <- Networks2],
    io:format("~s~n", [Jsons]),
    ok;
main(_) ->
    usage().

%% ===================================================================
%% Internal
%% ===================================================================

usage() ->
	ScriptName = escript:script_name(),
	BaseName = filename:basename(ScriptName),
    io:format("Usage: ~s <networks file> <prefixes file>~n", [BaseName]).

parse_networks_file(Filename) ->
    parse_file(Filename, fun(L) -> parse_network_line(L) end).

parse_prefixes_file(Filename) ->
    parse_file(Filename, fun(L) -> parse_prefix_line(L) end).

build_prefixes_dict(Prefixes) ->
    build_prefixes_dict(Prefixes, dict:new()).

build_prefixes_dict([P | Ps], Dict) ->
    NetworkId = P#prefix.network_id,
    Prefix = P#prefix.prefix,
    build_prefixes_dict(Ps, dict:append(NetworkId, Prefix, Dict));
build_prefixes_dict([], Dict) ->
    Dict.

set_prefixes_to_networks(Networks, PrefixesDict) ->
    set_prefixes_to_networks(Networks, PrefixesDict, []).

set_prefixes_to_networks([], _, Acc) ->
    lists:reverse(Acc);
set_prefixes_to_networks([N | Ns], Dict, Acc) ->
    NetworkId = N#network.'_id',
    N2 = case dict:find(NetworkId, Dict) of
            {ok, Prefixes} ->
                N#network{prefixes = {array, Prefixes}};
            error ->
                N
         end,
    set_prefixes_to_networks(Ns, Dict, [N2 | Acc]).

parse_file(Filename, Fun) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            [_ColumnNames | Lines] = string:tokens(binary_to_list(Bin), ?ROW_DELIM),
            ParsedLines = lists:map(Fun, Lines),
            {ok, ParsedLines};
        Error ->
            Error
    end.

parse_network_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,           Line2}  = parse_uuid(Line),
    {Country,      Line3}  = parse_string(Line2),
    {Operator,     Line4}  = parse_string(Line3),
    {HexCode,      Line5}  = parse_string(Line4),
    {DialCode,     Line6}  = parse_string(Line5),
    {NumberLen,    Line7}  = parse_integer(Line6),
    {GMTDiff,      Line8}  = parse_string(Line7),
    {DST,          Line9}  = parse_string(Line8),
    {_OrigSupport, Line10} = parse_string(Line9),
    {SMSProvID,    Line11} = parse_uuid(Line10),
    {_MMSProvID,   Line12} = parse_uuid(Line11),
    {SMSPoints,    Line13} = parse_float(Line12),
    {_MMSPoints,   Line14} = parse_float(Line13),
    {_CreatedBy,   Line15} = parse_string(Line14),
    {_CreatedOn,   Line16} = parse_string(Line15),
    {_ModifBy,     Line17} = parse_string(Line16),
    {_ModifOn,     Line18} = parse_string(Line17),
    {IsHome,           []} = parse_boolean(Line18),
    #network{
        '_id' = ID,
        name = Operator,
        country = Country,
        hex_code = HexCode,
        country_code = DialCode,
        number_len = NumberLen,
        gmt_diff = GMTDiff,
        dst = DST,
        provider_id = SMSProvID,
        is_home = IsHome,
        sms_points = SMSPoints
    }.

parse_prefix_line(Line) ->
    %io:format("~p~n", [Line]),
    {NetworkId, Line2} = parse_uuid(Line),
    {Prefix,       []} = parse_string(Line2),
    #prefix{
        network_id = NetworkId,
        prefix = Prefix
    }.

parse_uuid(Str) ->
    %io:format("uuid: ~p~n", [Str]),
    case parse_string(Str) of
        {{string, []}, Rest} ->
            {{string, []}, Rest};
        {{string, Value}, Rest} ->
            [UUID] = string:tokens(Value, "{}"),
            Uuid = string:to_lower(UUID),
            {{string, Uuid}, Rest}
    end.

parse_integer(Str) ->
    %io:format("integer: ~p~n", [Str]),
    {{string, Value}, Rest} = parse_string(Str),
    {{integer, list_to_integer(Value)}, Rest}.

parse_float(Str) ->
    %io:format("float: ~p~n", [Str]),
    {{string, Value}, Rest} = parse_string(Str),
    {{float, convert_to_float(Value)}, Rest}.

parse_boolean(Str) ->
    %io:format("boolean: ~p~n", [Str]),
    case parse_string(Str) of
        {{string, "1"}, Rest} ->
            {{boolean, true}, Rest};
        {{string, "0"}, Rest} ->
            {{boolean, false}, Rest}
    end.

parse_string([]) ->
    %io:format("string: []~n"),
    {{string, []}, []};
parse_string(Str) ->
    %io:format("string: ~p~n", [Str]),
    case split_field(Str, ?COL_DELIM, ?COL_WRAPPER) of
        {Value, [?COL_DELIM | []]} ->
            {{string, strip(Value, ?COL_WRAPPER)}, []};
        {Value, [?COL_DELIM | Rest]} ->
            {{string, strip(Value, ?COL_WRAPPER)}, Rest};
        {Value, []} ->
            {{string, strip(Value, ?COL_WRAPPER)}, []}
    end.

split_field([ColWrapper | Rest], _ColDelim, ColWrapper) ->
    {Field, [ColWrapper | Rest2]} =
        lists:splitwith(fun(C) -> C =/= ColWrapper end, Rest),
    {[ColWrapper] ++ Field ++ [ColWrapper], Rest2};
split_field(Str, ColDelim, _ColWrapper) ->
    lists:splitwith(fun(C) -> C =/= ColDelim end, Str).

strip(Str, Char) ->
    string:strip(string:strip(Str, both, Char)).

convert_to_float([]) ->
    0.0;
convert_to_float(List) ->
    try list_to_float(List)
    catch
        error:badarg ->
            try list_to_integer(List) of
                Int -> float(Int)
            catch
                error:badarg ->
                    case lists:suffix(".", List) of
                        true ->
                            convert_to_float(List ++ "0");
                        false ->
                            case lists:prefix(".", List) of
                                true ->
                                    convert_to_float("0" ++ List);
                                false ->
                                    erlang:error({bad_float, List})
                            end
                    end
            end
    end.

record_to_proplist(#network{} = Rec) ->
    lists:zip(record_info(fields, network), tl(tuple_to_list(Rec)));
record_to_proplist(#prefix{} = Rec) ->
    lists:zip(record_info(fields, prefix), tl(tuple_to_list(Rec))).

proplist_to_json(Plist) ->
    lists:flatten(lists:reverse(["}\n" | proplist_to_json(Plist, ["{"])])).

proplist_to_json([{Key, Value}], Acc) ->
    KeyValue = format_key_value(Key, Value),
    [KeyValue | Acc];
proplist_to_json([{Key, Value} | Plist], Acc) ->
    KeyValue = format_key_value(Key, Value),
    proplist_to_json(Plist, [",", KeyValue | Acc]).

format_key_value(Key, Value) ->
    [format_key(Key), ":", format_value(Value)].

format_key(Key) ->
    io_lib:format("~p", [atom_to_list(Key)]).

format_value({string, []}) ->
    "\"\"";
format_value({string, Value}) ->
    io_lib:format("~1000000p", [Value]);
format_value({integer, Value}) ->
    integer_to_list(Value);
format_value({array, []}) ->
    "[]";
format_value({array, Array}) ->
    Values = string:join([format_value(V) || V <- Array], ","),
    "[" ++ Values ++ "]";
format_value({boolean, Value}) ->
    atom_to_list(Value);
format_value({float, Value}) ->
    io_lib:format("~p", [Value]).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

split_field_1_test() ->
    Str = "\"1,2,3\",\"4\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"1,2,3\"", ",\"4\""},
    ?assertEqual(Expected, Actual).

split_field_2_test() ->
    Str = "1,2,3,4",
    Actual = split_field(Str, $,, $"),
    Expected = {"1", ",2,3,4"},
    ?assertEqual(Expected, Actual).

parse_prefix_1_test() ->
    PrefixLine = "\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"312100\"",
    Prefix = parse_prefix_line(PrefixLine),
    ExpPrefix = #prefix{
        network_id = {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"},
        prefix = {string, "312100"}
    },
    ?assertEqual(ExpPrefix, Prefix),

    Plist = record_to_proplist(Prefix),
    ExpPlist = [
        {network_id, {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"}},
        {prefix, {string, "312100"}}
    ],
    ?assertEqual(ExpPlist, Plist),

    Json = proplist_to_json(Plist),
    ExpJson = "{\"network_id\":\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"prefix\":\"312100\"}\n",
    ?assertEqual(ExpJson, Json).

parse_prefix_2_test() ->
    PrefixLine = "\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"\"",
    Prefix = parse_prefix_line(PrefixLine),
    ExpPrefix = #prefix{
        network_id = {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"},
        prefix = {string, []}
    },
    ?assertEqual(ExpPrefix, Prefix),

    Plist = record_to_proplist(Prefix),
    ExpPlist = [
        {network_id, {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"}},
        {prefix, {string, ""}}
    ],
    ?assertEqual(ExpPlist, Plist),

    Json = proplist_to_json(Plist),
    ExpJson = "{\"network_id\":\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"prefix\":\"\"}\n",
    ?assertEqual(ExpJson, Json).

parse_network_1_test() ->
    NetworkLine = "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"Albania\",\"Mobile Comms\",\"\",\"355\",\"0\",\"+1\",\"7,5,3;7,5,10\",\"1\",\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"\",\"5,0000\",\"5\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 12:34:59\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"13.01.2010 17:23:39\",\"0\"",
    Network = parse_network_line(NetworkLine),
    ExpNetwork = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, "7,5,3;7,5,10"},
        provider_id = {string, "65c4b123-2a51-49e1-84a8-b31dac878d8c"},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },
    ?assertEqual(ExpNetwork, Network),

    Plist = record_to_proplist(Network),
    Json = proplist_to_json(Plist),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[],\"gmt_diff\":\"+1\",\"dst\":\"7,5,3;7,5,10\",\"provider_id\":\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_credits\":1.0}\n",
    ?assertEqual(ExpJson, Json).

parse_network_2_test() ->
    NetworkLine = "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"Albania\",\"Mobile Comms\",\"\",\"355\",\"0\",\"+1\",\"\",\"1\",\"\",\"\",\"5,0000\",\"5\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 12:34:59\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"13.01.2010 17:23:39\",\"0\"",
    Network = parse_network_line(NetworkLine),
    ExpNetwork = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, ""},
        provider_id = {string, ""},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },
    ?assertEqual(ExpNetwork, Network),

    Plist = record_to_proplist(Network),
    Json = proplist_to_json(Plist),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[],\"gmt_diff\":\"+1\",\"dst\":\"\",\"provider_id\":\"\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_credits\":1.0}\n",
    ?assertEqual(ExpJson, Json).

build_prefixes_dict_test() ->
    Lines = [
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"663\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"664\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"669\""
    ],
    Prefixes = [parse_prefix_line(L) || L <- Lines],
    PrefixesDict = build_prefixes_dict(Prefixes),
    Actual = dict:fetch({string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"}, PrefixesDict),
    Expected = [{string, "663"}, {string, "664"}, {string, "669"}],
    ?assertEqual(Expected, Actual).

build_full_network_test() ->
    Lines = [
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"663\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"664\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"669\""
    ],
    Prefixes = [parse_prefix_line(L) || L <- Lines],
    PrefixesDict = build_prefixes_dict(Prefixes),

    Network = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, "7,5,3;7,5,10"},
        provider_id = {string, "65c4b123-2a51-49e1-84a8-b31dac878d8c"},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },

    [Network2] = set_prefixes_to_networks([Network], PrefixesDict),

    Plist = record_to_proplist(Network2),

    Json = proplist_to_json(Plist),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[\"663\",\"664\",\"669\"],\"gmt_diff\":\"+1\",\"dst\":\"7,5,3;7,5,10\",\"provider_id\":\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_credits\":1.0}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
