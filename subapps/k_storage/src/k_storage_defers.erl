-module(k_storage_defers).

-include("customer.hrl").
-include("msg_info.hrl").
-include_lib("alley_common/include/logging.hrl").

%% API
-export([
    get_all/4,
    get_one/1,
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
    case mongodb_storage:find(defers_storage, mt_defers, Selector, {}, Skip, Limit) of
        {ok, Docs} ->
            {ok, [k_storage_utils:doc_to_mt_batch_info(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

-spec get_one(req_id()) ->
    {ok, #batch_info{}} | {error, reason()}.
get_one(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {},
    case mongodb_storage:find_one(defers_storage, mt_defers, Selector, Projector) of
        {ok, Doc} ->
            {ok, k_storage_utils:doc_to_mt_batch_info(Doc)};
        {error, Error} ->
            {error, Error}
    end.

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
            'rqs', term_to_binary(SmsReq)
        }
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).
