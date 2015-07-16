-module(k_http_api_utils).

-export([
    decode_msisdn/1,
    decode_interface/1,
    decode_feature/1,

    prepare_features/1,
    prepare_originators/1,
    prepare_users/2
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("k_storage/include/msisdn.hrl").

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
        <<"oneapi">>      -> oneapi;
        <<"email">>       -> email
    end.

features() -> [
    {<<"override_originator">>, [<<"empty">>, <<"any">>, <<"false">>]},
    {<<"inbox">>, [<<"true">>, <<"false">>]},
    {<<"sms_from_email">>, [<<"true">>, <<"false">>]}
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
prepare_features(Feature = #feature{}) ->
    prepare_features([Feature], []).

prepare_features([], Acc) ->
    {ok, Acc};
prepare_features([Feature = #feature{} | Rest], Acc) ->
    Fun = ?record_to_proplist(feature),
    Plist = Fun(Feature),
    prepare_features(Rest, [Plist | Acc]).

-spec prepare_originators(#originator{}) -> {ok, [{atom(), term()}]}.
prepare_originators(Orig = #originator{}) ->
    AddrFun = ?record_to_proplist(addr),
    OrigFun = ?record_to_proplist(originator),
    AddrPlist = proplists:delete(ref_num, AddrFun(Orig#originator.address)),
    OrigPlist = proplists:delete(address, OrigFun(Orig)),
    [{msisdn, AddrPlist} | OrigPlist];
prepare_originators(Originators) when is_list(Originators) ->
    {ok, [prepare_originators(Originator) || Originator <- Originators]}.

-spec prepare_users(#customer{}, #user{}) -> {ok, [{atom(), term()}]}.
prepare_users(Customer, User = #user{features = Features}) ->
    {ok, FeaturesPlists} =
        k_http_api_utils:prepare_features(Features),
    UserFun = ?record_to_proplist(user),
    Plist = UserFun(User#user{features = FeaturesPlists}),
    Plist2 = [{user_id, ?gv(id, Plist)} | Plist],
    Plist3 = [{customer_uuid, Customer#customer.customer_uuid} | Plist2],
    Plist4 = [{customer_id, Customer#customer.customer_id} | Plist3],
    Plist5 = [{customer_name, Customer#customer.name} | Plist4],
    Plist6 = proplists:delete(password, Plist5),
    proplists:delete(id, Plist6);
prepare_users(Customer, Users) when is_list(Users) ->
    {ok, [prepare_users(Customer, User) || User <- Users]}.
