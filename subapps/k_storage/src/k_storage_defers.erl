-module(k_storage_defers).

-include("customer.hrl").
-include("msg_info.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").

%% API
-export([
    get_all/4,
    get_one/1,
    get_expired_up_to/1,
    delete/1,
    delete/2,
    set_mt_def_batch_info/2
]).

-type reason() :: any().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all(customer_id(), user_id(), pos_integer(), pos_integer()) ->
    {ok, #batch_info{}} | {error, reason()}.
get_all(CustomerUuid, UserId, Skip, Limit) ->
    CustomerUuidSel =
        case CustomerUuid of
            undefined -> [];
            CustomerId -> [{'ci', CustomerId}]
        end,
    UserIdSel =
        case UserId of
            undefined -> [];
            UserId -> [{'ui', UserId}]
        end,
    Selector =
        {'$query',
            bson:document(
                CustomerUuidSel ++ UserIdSel
            )
        },
    Projector = {'rqs', 0},
    case mongodb_storage:find(defers_storage, mt_defers, Selector, Projector, Skip, Limit) of
        {ok, Docs} ->
            {ok, [k_storage_utils:doc_to_mt_batch_info(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

-spec get_one(req_id()) ->
    {ok, #batch_info{}} | {error, reason()}.
get_one(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {'rqs', 0},
    case mongodb_storage:find_one(defers_storage, mt_defers, Selector, Projector) of
        {ok, Doc} ->
            {ok, k_storage_utils:doc_to_mt_batch_info(Doc)};
        {error, Error} ->
            {error, Error}
    end.

-spec get_expired_up_to(os:timestamp()) ->
    {ok, [#batch_info{}]} | {error, reason()}.
get_expired_up_to(Ts) ->
    Selector = {
        'dft', {'$lte', Ts}
    },
    Projector = {
        'rqs', 1
    },
    case mongodb_storage:find(defers_storage, mt_defers, Selector, Projector) of
        {ok, Docs} ->
            {ok, [reformat_request(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

-spec delete(req_id()) -> ok | {error, reason()}.
delete(ReqId) ->
    mongodb_storage:delete(defers_storage, mt_defers, {'_id', ReqId}).

-spec delete(req_id(), gateway_id()) -> ok | {error, reason()}.
delete(ReqId, GtwId) ->
    Selector = {
        '_id', ReqId
    },
    Modifier = {
        '$pull', {
            'rqs', {'gi', GtwId}
        }
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).

-spec set_mt_def_batch_info(#batch_info{}, term()) -> ok | {error, reason()}.
set_mt_def_batch_info(#batch_info{
    req_id = ReqId,
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
    def_time = DefTime,
    src_addr = SrcAddr,
    encoding = Encoding,
    body = Body,
    reg_dlr = RegDlr,
    esm_class = EsmClass,
    val_period = ValPeriod,
    req_time = ReqTime,
    recipients = Recipients,
    messages = Messages,
    price = Price
}, SmsReq) ->
    Selector = {
        '_id', ReqId
    },
    Modifier = {
        '$setOnInsert', {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'dft', DefTime,
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        },
        '$inc', {
            'rs' , Recipients,
            'ms' , Messages,
            'p'  , Price
        },
        '$push', {
            'rqs', {
                'gi', gateway_id(SmsReq),
                'rq', term_to_binary(SmsReq)
            }
        }
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

gateway_id(#sms_req_v1{} = SmsReq) ->
    SmsReq#sms_req_v1.gateway_id.

reformat_request(Doc) ->
    ReqId = bsondoc:at('_id', Doc),
    Reqs = [{bsondoc:at('gi', R), bsondoc:at('rq', R)}
            || R <- bsondoc:at('rqs', Doc)],
    {ReqId, Reqs}.
