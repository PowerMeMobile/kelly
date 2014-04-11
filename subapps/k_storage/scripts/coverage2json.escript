#!/usr/bin/env escript

-export([main/1]).

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

-define(ROW_DELIM, "\n").
-define(COL_DELIM, $|).

%% ===================================================================
%% API
%% ===================================================================

-spec main(list()) -> no_return().
main([NetworksFilename, PrefixesFilename]) ->
    %% PrefixLine1 = "{ACDDF70B-83D9-43E1-9842-74891D04ECE0}|27",
    %% Prefix1 = parse_prefix_line(PrefixLine1),
    %% io:format("~p~n", [Prefix1]),
    %% Plist1 = record_to_proplist(Prefix1),
    %% io:format("~p~n", [Plist1]),
    %% Json1 = proplist_to_json(Plist1),
    %% io:format("~s~n", [Json1]),

    %% PrefixLine2 = "{ACDDF70B-83D9-43E1-9842-74891D04ECE0}|",
    %% Prefix2 = parse_prefix_line(PrefixLine2),
    %% io:format("~p~n", [Prefix2]),
    %% Plist2 = record_to_proplist(Prefix2),
    %% io:format("~p~n", [Plist2]),
    %% Json2 = proplist_to_json(Plist2),
    %% io:format("~s~n", [Json2]),

    %% NetworkLine1 = "{AAE6265D-965E-4A72-A79E-7DDBC411DAB5}|Albania|Mobile Comms|72F610|355|0|+1|7,5,3;7,5,10|1|{E5ECBB4F-8D1E-418D-A133-E8E35E9287B7}|{E5ECBB4F-8D1E-418D-A133-E8E35E9287B7}|1|1|{AE3B4951-92AE-41C5-83B0-6F33F1FD57B9}|2008-05-12 12:44:28.943000000|{AE3B4951-92AE-41C5-83B0-6F33F1FD57B9}|2009-02-20 14:02:26.827000000|0",
    %% Network1 = parse_network_line(NetworkLine1),
    %% io:format("~p~n", [Network1]),
    %% Plist1 = record_to_proplist(Network1),
    %% io:format("~p~n", [Plist1]),
    %% Json1 = proplist_to_json(Plist1),
    %% io:format("~s~n", [Json1]),

    %% NetworkLine2 = "{BF86893E-445A-4435-92DF-95105B55E4DF}|Bangladesh|SHEBA TELECOM|74F030|880|8|+6||1|||1|1|{AE3B4951-92AE-41C5-83B0-6F33F1FD57B9}|2009-02-20 14:02:30.450000000|||0",
    %% Network2 = parse_network_line(NetworkLine2),
    %% io:format("~p~n", [Network2]),
    %% Plist2 = record_to_proplist(Network2),
    %% io:format("~p~n", [Plist2]),
    %% Json2 = proplist_to_json(Plist2),
    %% io:format("~s~n", [Json2]),

    {ok, Prefixes} = parse_prefixes_file(PrefixesFilename),
    %io:format("~p~n", [Prefixes]),
    {ok, Networks} = parse_networks_file(NetworksFilename),
    %io:format("~p~n", [Networks]),

    PrefixesDict = build_prefixes_dict(Prefixes),
    %io:format("~p~n", [dict:fetch({string, "22525437-51e8-4ce4-b475-7543b0c1286b"}, PrefixesDict)]),

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
    {Id,           Line2}  = parse_uuid(Line),
    {Country,      Line3}  = parse_string(Line2),
    {Operator,     Line4}  = parse_string(Line3),
    {HexCode,      Line5}  = parse_string(Line4),
    {DialCode,     Line6}  = parse_string(Line5),
    {NumberLen,    Line7}  = parse_integer(Line6),
    {GMTDiff,      Line8}  = parse_string(Line7),
    {DST,          Line9}  = parse_string(Line8),
    {_OrigSupport, Line10} = parse_string(Line9),
    {SmsProvId,    Line11} = parse_uuid(Line10),
    {_MmsProvId,   Line12} = parse_uuid(Line11),
    {SmsPoints,    Line13} = parse_float(Line12),
    {_MmsPoints,   Line14} = parse_float(Line13),
    {_CreatedBy,   Line15} = parse_string(Line14),
    {_CreatedOn,   Line16} = parse_string(Line15),
    {_ModifBy,     Line17} = parse_string(Line16),
    {_ModifOn,     Line18} = parse_string(Line17),
    {IsHome,           []} = parse_boolean(Line18),
    #network{
        '_id' = Id,
        name = Operator,
        country = Country,
        hex_code = HexCode,
        country_code = DialCode,
        number_len = NumberLen,
        gmt_diff = GMTDiff,
        dst = DST,
        provider_id = SmsProvId,
        is_home = IsHome,
        sms_points = SmsPoints
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
    case lists:splitwith(fun(C) -> C =/= ?COL_DELIM end, Str) of
        {Value, [$| | []]} ->
            {{string, Value}, []};
        {Value, [$| | Rest]} ->
            {{string, Value}, Rest};
        {Value, []} ->
            {{string, Value}, []}
    end.

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
