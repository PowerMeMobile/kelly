%% TODO: move snmp specific logic to k_control
-module(k_http_api_handler_gateways_settings).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/gateway.hrl").

-define(GTW_ID_PARAM,
    #param{
        name = gateway_id,
        mandatory = true,
        repeated = false,
        type = binary
    }).
-define(SETTING_ID_PARAM(Mandatory),
    (fun() ->
        case Mandatory of
            true ->
                #param{
                    name = name,
                    mandatory = true,
                    repeated = false,
                    type = binary
                };
            false ->
                #param{
                    name = name,
                    mandatory = false,
                    repeated = false,
                    type = binary
                }
        end
    end)()).

-define(SETTING_VALUE_PARAM,
    #param{
        name = value,
        mandatory = false,
        repeated = false,
        type = binary
    }).

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        ?GTW_ID_PARAM,
        ?SETTING_ID_PARAM(false)
    ],
    Update = [
        ?GTW_ID_PARAM,
        ?SETTING_ID_PARAM(true),
        ?SETTING_VALUE_PARAM
    ],
    Delete = [
        ?GTW_ID_PARAM,
        ?SETTING_ID_PARAM(true)
    ],
    Create = [
        ?GTW_ID_PARAM,
        ?SETTING_ID_PARAM(true),
        ?SETTING_VALUE_PARAM
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/gateways/:gateway_id/settings/[:name]"
    }}.

read(Params) ->
    ?log_debug("Read gtw settings (params: ~p)", [Params]),
    SettingId = ?gv(name, Params),
    case SettingId of
        undefined -> read_all(?gv(gateway_id, Params));
        _ -> read_id(?gv(gateway_id, Params), SettingId)
    end.

create(Params) ->
    GtwId = ?gv(gateway_id, Params),
    case k_storage_gateways:get_gateway(GtwId) of
        {ok, Gtw = #gateway{}} ->
            create_setting(validate, Gtw, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update(Params) ->
    GtwId = ?gv(gateway_id, Params),
    case k_storage_gateways:get_gateway(GtwId) of
        {ok, Gtw = #gateway{}} ->
            update_setting(validate, Gtw, Params);
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

delete(Params) ->
    GtwId = ?gv(gateway_id, Params),
    SettingId = ?gv(name, Params),
    case k_storage_gateways:get_gateway(GtwId) of
        {ok, Gtw = #gateway{settings = Settings}} ->
            NewSettings = lists:keydelete(SettingId, #setting.name, Settings),
            ok = k_control_just:delete_setting(GtwId, SettingId),
            ok = k_storage_gateways:set_gateway(GtwId, Gtw#gateway{settings = NewSettings}),
            {ok, StsPropList} = prepare_settings(Settings),
            ?log_debug("StsPropList: ~p", [StsPropList]),
            {http_code, 204};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

read_all(GatewayId) ->
    ?log_debug("Try to search gtw ~p", [GatewayId]),
    case k_storage_gateways:get_gateway(GatewayId) of
        {ok, #gateway{settings = Settings}} ->
            {ok, StsPropList} = prepare_settings(Settings),
            ?log_debug("StsPropList: ~p", [StsPropList]),
            {http_code, 200, StsPropList};
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

read_id(GatewayId, StsName) ->
    case k_storage_gateways:get_gateway(GatewayId) of
        {ok, #gateway{settings = Settings}} ->
            case get_setting(StsName, Settings) of
                false ->
                    ?log_debug("Sts [~p] not found", [StsName]),
                    {exception, 'svc0003'};
                Sts = #setting{} ->
                    {ok, [StsPropList]} = prepare_settings(Sts),
                    ?log_debug("StsPropList: ~p", [StsPropList]),
                    {http_code, 200, StsPropList}
            end;
        {error, no_entry} ->
            {exception, 'svc0003'}
    end.

update_setting(validate, Gtw, Params) ->
    case validate(?gv(name, Params), ?gv(value, Params)) of
        error -> {exception, 'svc0001', [?gv(name, Params)]};
        _ -> update_setting(update, Gtw, Params)
    end;
update_setting(update, Gtw, Params) ->
    SettingId = ?gv(name, Params),
    GtwId = ?gv(gateway_id, Params),
    #gateway{settings = Settings} = Gtw,
    case get_setting(SettingId, Settings) of
        false -> {exception, 'svc0003'};
        Setting = #setting{} ->
            NewValue = ?resolve(value, Params, Setting#setting.value),
            NewSetting = #setting{
                name = SettingId,
                value = NewValue
            },
            NewSettings = lists:keyreplace(SettingId, #setting.name, Settings, NewSetting),
            ok = k_control_just:set_setting(GtwId, NewSetting),
            ok = k_storage_gateways:set_gateway(GtwId, Gtw#gateway{settings = NewSettings}),
            {ok, [StsPropList]} = prepare_settings(NewSetting),
            ?log_debug("StsPropList: ~p", [StsPropList]),
            {http_code, 200, StsPropList}
    end.

get_setting(StsName, StsList) ->
    lists:keyfind(StsName, #setting.name, StsList).

create_setting(validate, Gtw, Params) ->
    case validate(?gv(name, Params), ?gv(value, Params)) of
        error -> {exception, 'svc0001', [?gv(name, Params)]};
        _ -> create_setting(is_exist, Gtw, Params)
    end;
create_setting(is_exist, GTW = #gateway{settings = Settings}, Params) ->
    SettingId = ?gv(name, Params),
    case get_setting(SettingId, Settings) of
        #setting{} ->
            ?log_debug("Setting [~p] already exist", [SettingId]),
            {exception, 'svc0004'};
        false ->
            create_setting(create, GTW, Params)
    end;
create_setting(create, GTW, Params) ->
    Name = ?gv(name, Params),
    Value = ?gv(value, Params),
    Setting = #setting{
        name = Name,
        value = Value
    },
    GtwId = ?gv(gateway_id, Params),
    ok = k_control_just:set_setting(GtwId, Setting),
    ok = k_storage_gateways:set_gateway(GtwId, GTW#gateway{settings = [Setting | GTW#gateway.settings]}),
    {ok, [StsPropList]} = prepare_settings(Setting),
    ?log_debug("StsPropList: ~p", [StsPropList]),
    {http_code, 201, StsPropList}.

prepare_settings(SettingsList) when is_list(SettingsList) ->
    prepare_settings(SettingsList, []);
prepare_settings(Setting = #setting{}) ->
    prepare_settings([Setting], []).

prepare_settings([], Acc) ->
    {ok, Acc};
prepare_settings([Setting = #setting{} | Rest], Acc) ->
    StsFun = ?record_to_proplist(setting),
    StsPropList = StsFun(Setting),
    prepare_settings(Rest, [StsPropList | Acc]).

%% ===================================================================
%% Validation
%% ===================================================================

validate(Name, Value) when is_binary(Name) andalso is_binary(Value) ->
    cast({binary_to_list(Name), binary_to_list(Value)}).

cast({"max_validity_period", Value}) ->
    cast_pos_integer(Value);
cast({Name, Value}) when Name =:= "smpp_enquire_link_time";
                         Name =:= "smpp_response_time" ->
    cast_pos_integer(Value);
cast({"smpp_inactivity_time", Value}) ->
    case Value of
        "infinity" -> infinity;
        _          -> cast_pos_integer(Value)
    end;
cast({"smpp_version", Value}) ->
    case Value of
        "3.3" -> 16#33;
        "3.4" -> 16#34;
        "5.0" -> 16#50;
        _     -> error
    end;
cast({"sar_method", Value}) when Value =:= "udh";
                                 Value =:= "sar_tlv" ->
    list_to_atom(Value);
cast({"ip", Value}) ->
    case inet_parse:address(Value) of
        {ok, IpAddr} ->
            IpAddr;
        {error, einval} ->
            error
    end;
cast({"max_reconnect_delay", Value}) ->
    cast_pos_integer(Value);
cast({"smpp_window_size", Value}) ->
    cast_pos_integer(Value);
cast({"log_smpp_pdus", Value}) when Value =:= "true";
                                    Value =:= "false" ->
    list_to_atom(Value);
%% SETTINGS THAT MIGHT NOT SURVIVE REFACTORING
cast({"default_encoding", Value}) when Value =:= "gsm0338";
                                       Value =:= "ascii";
                                       Value =:= "latin1";
                                       Value =:= "ucs2" ->
    list_to_atom(Value);
cast({"default_data_coding", Value}) ->
    cast_non_neg_integer(Value);
cast({"default_bitness", Value}) when Value =:= "7";
                                      Value =:= "8";
                                      Value =:= "16" ->
    list_to_integer(Value);
cast({Name, Value}) when Name =:= "terminal_errors";
                         Name =:= "discarded_errors" ->
    Split = string:tokens(Value, ","),
    case lists:all(fun(P) -> nomatch =/= re:run(P, "^[0-9A-Fa-f]+$") end, Split) of
        true ->
            [ erlang:list_to_integer(E, 16) || E <- Split ];
        false ->
            error
    end;
cast({Name, Value}) when Name =:= "fast_retry_times";
                         Name =:= "slow_retry_times";
                         Name =:= "fast_retry_delay";
                         Name =:= "slow_retry_delay" ->
    cast_non_neg_integer(Value);
cast({"smpp_priority_flag", Value}) ->
    case cast_non_neg_integer(Value) of
        N when is_integer(N), N >= 0, N =< 3 ->
            N;
        _ ->
            error
    end;
cast({"smpp_protocol_id", Value}) ->
    cast_non_neg_integer(Value);
cast({_Name, _Value}) ->
    error.

cast_pos_integer(Value) ->
    case cast_integer(Value) of
        N when is_integer(N), N > 0 ->
            N;
        _ ->
            error
    end.

cast_non_neg_integer(Value) ->
    case cast_integer(Value) of
        N when is_integer(N), N >= 0 ->
            N;
        _ ->
            error
    end.

cast_integer(Value) when is_list(Value) ->
    try
        list_to_integer(Value)
    catch
        error:badarg ->
            error
    end.
