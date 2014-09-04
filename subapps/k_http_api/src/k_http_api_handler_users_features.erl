-module(k_http_api_handler_users_features).

-behaviour(gen_http_api).

-export([
    init/0,
    create/1,
    read/1,
    update/1,
    delete/1
]).

%% export helpers
-export([
    prepare_features/1
]).

-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("gen_http_api/include/crud_specs.hrl").
-include_lib("k_storage/include/customer.hrl").

%% ===================================================================
%% Callback Functions
%% ===================================================================

init() ->
    Read = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = false, repeated = false, type = binary}
    ],
    Update = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = value, mandatory = true, repeated = false, type = binary}
    ],
    Delete = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary}
    ],
    Create = [
        #param{name = customer_uuid, mandatory = true, repeated = false, type = binary},
        #param{name = user_id, mandatory = true, repeated = false, type = binary},
        #param{name = name, mandatory = true, repeated = false, type = binary},
        #param{name = value, mandatory = true, repeated = false, type = binary}
    ],
    {ok, #specs{
        create = Create,
        read = Read,
        update = Update,
        delete = Delete,
        route = "/customers/:customer_uuid/users/:user_id/features/[:name]"
    }}.

read(Params) ->
    with_params_do(Params, fun(Name, _Value, Features) ->
        case Name of
            undefined ->
                return_all;
            Name ->
                case lists:keyfind(Name, #feature.name, Features) of
                    false ->
                        ?log_error("Feature not found: ~p", [Name]),
                        {exception, 'svc0003'};
                    Feature ->
                        {return_one, Feature}
                end
        end
    end).

create(Params) ->
    with_params_do(Params, fun(Name, Value, Features) ->
        create_feature(validate, Name, Value, Features)
    end).

update(Params) ->
    with_params_do(Params, fun(Name, Value, Features) ->
        update_feature(validate, Name, Value, Features)
    end).

delete(Params) ->
    with_params_do(Params, fun(Name, _Value, Features) ->
        Features2 = lists:keydelete(Name, #feature.name, Features),
        {save, Features2}
    end).

%% ===================================================================
%% Internal & API
%% ===================================================================

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

%% ===================================================================
%% Internal
%% ===================================================================

with_params_do(Params, Handler) ->
    CustomerUuid = ?gv(customer_uuid, Params),
    case k_storage_customers:get_customer_by_uuid(CustomerUuid) of
        {ok, #customer{users = Users} = Customer} ->
            UserId = ?gv(user_id, Params),
            case lists:keyfind(UserId, #user.id, Users) of
                false ->
                    ?log_error("User not found (customer_uuid: ~p, user_id: ~p)",
                        [CustomerUuid, UserId]),
                    {exception, 'svc0003'};
                User ->
                    Name = ?gv(name, Params),
                    Value = ?gv(value, Params),
                    Features = User#user.features,

                    case Handler(Name, Value, Features) of
                        return_all ->
                            {ok, Plist} = prepare_features(Features),
                            ?log_debug("Features: ~p", [Plist]),
                            {http_code, 200, Plist};
                        {return_one, Feature} ->
                            {ok, [Plist]} = prepare_features(Feature),
                            ?log_debug("Feature: ~p", [Plist]),
                            {http_code, 200, Plist};
                        {save, Features2} ->
                            User2 = User#user{features = Features2},
                            Users2 = lists:keyreplace(UserId, #user.id, Users, User2),
                            Customer2 = Customer#customer{users = Users2},
                            ok = k_storage_customers:set_customer(CustomerUuid, Customer2),
                            {ok, Plist} = prepare_features(Features2),
                            ?log_debug("Features: ~p", [Plist]),
                            {http_code, 200, Plist};
                        {exception, Exc} ->
                            {exception, Exc};
                        {exception, Exc, Param} ->
                            {exception, Exc, Param}
                    end
            end;
        {error, no_entry} ->
            ?log_error("Customer not found (customer_uuid: ~p)", [CustomerUuid]),
            {exception, 'svc0003'}
    end.

update_feature(validate, Name, Value, Features) ->
    case validate(Name, Value) of
        error ->
            ?log_error("Feature invalid: ~p, ~p", [Name, Value]),
            {exception, 'svc0001', [Name]};
        ok ->
            update_feature(update, Name, Value, Features)
    end;
update_feature(update, Name, Value, Features) ->
    case lists:keyfind(Name, #feature.name, Features) of
        false ->
            ?log_error("Feature not found: ~p", [Name]),
            {exception, 'svc0003'};
        Feature ->
            Features2 = lists:keyreplace(Name, #feature.name,
                Features, Feature#feature{value = Value}),
            {save, Features2}
    end.

create_feature(validate, Name, Value, Features) ->
    case validate(Name, Value) of
        error ->
            ?log_error("Feature is invalid: ~p, ~p", [Name, Value]),
            {exception, 'svc0001', [Name]};
        ok ->
            create_feature(does_exist, Name, Value, Features)
    end;
create_feature(does_exist, Name, Value, Features) ->
    case lists:keyfind(Name, #feature.name, Features) of
        false ->
            create_feature(create, Name, Value, Features);
        _Feature ->
            ?log_error("Feature already exists: ~p", [Name]),
            {exception, 'svc0004'}
    end;
create_feature(create, Name, Value, Features) ->
    Features2 = [#feature{name = Name, value = Value} | Features],
    {save, Features2}.

%% ===================================================================
%% Validation
%% ===================================================================

validate(Name, Value) when is_binary(Name) andalso is_binary(Value) ->
    cast({binary_to_list(Name), binary_to_list(Value)}).

cast({"override_originator", Value}) when Value =:= "empty";
                                          Value =:= "any";
                                          Value =:= "false" ->
    ok;
cast({"inbox", Value}) when Value =:= "true";
                            Value =:= "false" ->
    ok;
cast({_Name, _Value}) ->
    error.
