-module(k_http_api_utils).

-export([
    decode_msisdn/1,
    decode_interface/1,
    decode_feature/1,
    decode_routing/1,

    prepare_features/1,
    prepare_originators/2,
    prepare_users/2,

    get_disabled_interfaces/2,
    get_disabled_feature_names/2,
    remove_features/2
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("k_storage/include/msisdn.hrl").

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec decode_msisdn(binary()) -> undefined | #addr{}.
decode_msisdn(<<>>) ->
    undefined;
decode_msisdn(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.

-spec decode_interface(binary()) -> interface().
decode_interface(Interface) ->
    case bstr:lower(Interface) of
        <<"transmitter">> -> transmitter;
        <<"receiver">>    -> receiver;
        <<"transceiver">> -> transceiver;
        <<"soap">>        -> soap;
        <<"mm">>          -> mm;
        <<"webmm">>       -> webmm;
        <<"oneapi">>      -> oneapi;
        <<"email">>       -> email
    end.

features() -> [
    {<<"override_originator">>, [<<"empty">>, <<"any">>, <<"false">>]},
    {<<"inbox">>, [<<"true">>, <<"false">>]},
    {<<"sms_from_email">>, [<<"true">>, <<"false">>]},
    {<<"bypass_blacklist">>, [<<"true">>, <<"false">>]}
].

-spec decode_feature(binary()) -> #feature{}.
decode_feature(Binary) ->
    [Name, Value] = binary:split(Binary, <<",">>),
    case proplists:get_value(Name, features()) of
        undefined ->
            error(unknown_feature_name);
        Values ->
            case lists:member(Value, Values) of
                false ->
                    error(unknown_feature_value);
                true ->
                    #feature{name = Name, value = Value}
            end
    end.

-spec prepare_features(#feature{}) -> {ok, [{atom(), term()}]}.
prepare_features(FeaturesList) when is_list(FeaturesList) ->
    prepare_features(FeaturesList, []);
prepare_features(F = #feature{}) ->
    prepare_features([F], []).

prepare_features([], Acc) ->
    {ok, lists:reverse(Acc)};
prepare_features([F = #feature{} | Fs], Acc) ->
    Fun = ?record_to_proplist(feature),
    prepare_features(Fs, [Fun(F) | Acc]).

-spec decode_routing(binary()) -> #routing{}.
decode_routing(Binary) ->
    [NetMapId, DefProvId] = binary:split(Binary, <<",">>),
    #routing{
        network_map_id = NetMapId,
        default_provider_id = check_undefined(DefProvId)
    }.

-spec prepare_routings(#routing{}) -> {ok, [{atom(), term()}]}.
prepare_routings(RoutingsList) when is_list(RoutingsList) ->
    prepare_routings(RoutingsList, []);
prepare_routings(F = #routing{}) ->
    prepare_routings([F], []).

prepare_routings([], Acc) ->
    {ok, lists:reverse(Acc)};
prepare_routings([R = #routing{} | Rs], Acc) ->
    Fun = ?record_to_proplist(routing),
    prepare_routings(Rs, [Fun(R) | Acc]).

-spec prepare_originators(#customer{}, #originator{}) -> {ok, [{atom(), term()}]}.
prepare_originators(Customer, Orig = #originator{routings = Routings}) ->
    AddrFun = ?record_to_proplist(addr),
    AddrPlist = proplists:delete(ref_num, AddrFun(Orig#originator.address)),
    {ok, RoutingsPlists} = prepare_routings(Routings),
    OrigFun = ?record_to_proplist(originator),
    Plist = OrigFun(Orig#originator{routings = RoutingsPlists}),
    Plist2 = [{msisdn, AddrPlist} | Plist],
    Plist3 = [{customer_uuid, Customer#customer.customer_uuid} | Plist2],
    Plist4 = [{customer_id, Customer#customer.customer_id} | Plist3],
    Plist5 = [{customer_name, Customer#customer.name} | Plist4],
    proplists:delete(address, Plist5);
prepare_originators(Customer, Originators) when is_list(Originators) ->
    {ok, [prepare_originators(Customer, O) || O <- Originators]}.

-spec prepare_users(#customer{}, #user{}) -> {ok, [{atom(), term()}]}.
prepare_users(Customer, User = #user{features = Features}) ->
    {ok, FeaturesPlists} = prepare_features(Features),
    UserFun = ?record_to_proplist(user),
    Plist = UserFun(User#user{features = FeaturesPlists}),
    Plist2 = [{user_id, ?gv(id, Plist)} | Plist],
    Plist3 = [{customer_uuid, Customer#customer.customer_uuid} | Plist2],
    Plist4 = [{customer_id, Customer#customer.customer_id} | Plist3],
    Plist5 = [{customer_name, Customer#customer.name} | Plist4],
    Plist6 = proplists:delete(password, Plist5),
    proplists:delete(id, Plist6);
prepare_users(Customer, Users) when is_list(Users) ->
    {ok, [prepare_users(Customer, U) || U <- Users]}.

-spec get_disabled_interfaces([interface()], [interface()]) -> [interface()].
get_disabled_interfaces(PreIfs, NewIfs) ->
    PreIfs -- NewIfs.

-spec get_disabled_feature_names([#feature{}], [#feature{}]) -> [binary()].
get_disabled_feature_names(PreFs, NewFs) ->
    Fs = lists:filter(
        fun(Feature=#feature{name=Name}) ->
            case lists:keyfind(Name, #feature.name, NewFs) of
                Feature ->
                    %% feature exists
                    %% filter it out
                    false;
                _Otherwise ->
                    %% feature was either removed or disabled
                    %% leave it
                    true
            end
        end,
        PreFs
    ),
    [N || #feature{name = N} <- Fs].

-spec remove_features([#feature{}], [binary()]) -> [#feature{}].
remove_features(Features, DisNames) ->
    [F || F <- Features, not lists:member(F#feature.name, DisNames)].

check_undefined(<<>>) ->
    undefined;
check_undefined(Value) ->
    Value.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

get_disabled_interfaces_test() ->
    PreIfs = [soap,mm,email],
    NewIfs = [soap],
    DisIfs = [mm,email],
    ?assertEqual(DisIfs, get_disabled_interfaces(PreIfs, NewIfs)).

get_disabled_feature_names_test() ->
    PreFs = [
        #feature{name = <<"f1">>, value = <<"true">>},
        #feature{name = <<"f2">>, value = <<"true">>},
        #feature{name = <<"f3">>, value = <<"true">>},
        #feature{name = <<"f4">>, value = <<"false">>}
    ],
    NowFs = [
        #feature{name = <<"f1">>, value = <<"false">>},
        #feature{name = <<"f3">>, value = <<"true">>}
    ],
    DisFs = [
        <<"f1">>,
        <<"f2">>,
        <<"f4">>
    ],
    ?assertEqual(DisFs, get_disabled_feature_names(PreFs, NowFs)).

remove_features_test() ->
    Fs = [
        #feature{name = <<"f1">>, value = <<"true">>},
        #feature{name = <<"f2">>, value = <<"true">>},
        #feature{name = <<"f3">>, value = <<"true">>},
        #feature{name = <<"f4">>, value = <<"false">>}
    ],
    DisNames = [
        <<"f1">>,
        <<"f2">>,
        <<"f4">>
    ],
    LeftFs = [
        #feature{name = <<"f3">>, value = <<"true">>}
    ],
    ?assertEqual(LeftFs, remove_features(Fs, DisNames)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
